{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
module JSONRPC.Server.Core where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Catch (
  MonadCatch,
  MonadMask,
  MonadThrow,
 )
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Dependent.Map qualified as DMap
import Data.GADT.Compare (GCompare)
import Data.Kind
import Data.Monoid (Ap (..))
import Data.Singletons
import JSONRPC.Types.Message
import JSONRPC.Types.Method
import JSONRPC.RPC qualified as RPC
import Control.Concurrent

newtype ServerT r k f m a = ServerT {unRpcT :: ReaderT (ServerEnv r k f) m a}
  deriving newtype (Functor, Applicative, Monad, MonadCatch, MonadIO, MonadMask, MonadThrow, MonadReader (ServerEnv r k f), MonadTrans, MonadUnliftIO, MonadFix)
  deriving (Semigroup, Monoid) via (Ap (ServerT r k f m) a)

-- for deriving the instance of MonadUnliftIO
type role ServerT nominal nominal nominal representational nominal

runServerT :: ServerEnv r k f -> ServerT r k f m a -> m a
runServerT env = flip runReaderT env . unRpcT
{-# INLINE runServerT #-}

type RpcM r k = ServerT r k IO IO

class RPC.MonadRpc r k (ResponseCallback r k f) m => MonadServer r k f m | m -> r k f where
  getServerEnv :: m (ServerEnv r k f)

instance MonadIO m => RPC.MonadRpc r k (ResponseCallback r k f) (ServerT r k f m) where
  getRpcEnv = resRpcEnv <$> getServerEnv

instance MonadIO m => MonadServer r k f (ServerT r k f m) where
  getServerEnv = ask

data ServerEnv r k f = ServerEnv
  { resHandlers :: TVar (Handlers r k f)
  , resRpcEnv :: RPC.RpcEnv r k (ResponseCallback r k f)
  }

-- ---------------------------------------------------------------------
-- Handlers
-- ---------------------------------------------------------------------

data ResponseCallback (r :: Role) k (f :: Type -> Type) (m :: k) where
  ResponseCallback :: (RequestOk r m) => (Either (ResponseError m) (MessageResult m) -> f ()) -> ResponseCallback r k f m

data RequestHandler r k (f :: Type -> Type) (m :: k) where
  RequestHandler :: (RequestOk (OtherRole r) m) => (RequestMessage m -> f (Either (ResponseError m) (MessageResult m))) -> RequestHandler r k f m

data NotificationHandler r k (f :: Type -> Type) (m :: k) where
  NotificationHandler :: (NotificationOk (OtherRole r) m) => (NotificationMessage m -> f ()) -> NotificationHandler r k f m

data Handlers r k (f :: Type -> Type) = Handlers
  { reqHandlers :: !(DMap.DMap (Sing @k) (RequestHandler r k f))
  , notHandlers :: !(DMap.DMap (Sing @k) (NotificationHandler r k f))
  }

instance (GCompare (Sing @k)) => Semigroup (Handlers r k f) where
  Handlers r1 n1 <> Handlers r2 n2 = Handlers (r1 <> r2) (n1 <> n2)
instance (GCompare (Sing @k)) => Monoid (Handlers r k f) where
  mempty = Handlers mempty mempty

notificationHandler :: forall r k (m :: k) f. (Method k) => Sing m -> NotificationHandler r k f m -> Handlers r k f
notificationHandler m h = Handlers mempty (DMap.singleton m h)

requestHandler :: forall r k (m :: k) f. (Method k) => Sing m -> RequestHandler r k f m -> Handlers r k f
requestHandler m h = Handlers (DMap.singleton m h) mempty

-- | How to convert two isomorphic data structures between each other.
data m <~> n = Iso
  { forward :: forall a. m a -> n a
  , backward :: forall a. n a -> m a
  }

transmuteHandlers :: forall r k m n . (m <~> n) -> Handlers r k m -> Handlers r k n
transmuteHandlers nat = mapHandlers mapReqs mapNots
  where
    mapReqs :: forall (a :: k). RequestHandler r k m a -> RequestHandler r k n a
    mapReqs (RequestHandler f) = RequestHandler $ \msg -> forward nat (f msg)
    mapNots :: forall (a :: k). NotificationHandler r k m a -> NotificationHandler r k n a
    mapNots (NotificationHandler f) = NotificationHandler $ \msg -> forward nat (f msg)

mapHandlers ::
  forall r k m n.
  (forall (a :: k). RequestHandler r k m a -> RequestHandler r k n a) ->
  (forall (a :: k). NotificationHandler r k m a -> NotificationHandler r k n a) ->
  Handlers r k m ->
  Handlers r k n
mapHandlers mapReq mapNot (Handlers reqs nots) = Handlers reqs' nots'
 where
  reqs' = DMap.map mapReq reqs
  nots' = DMap.map mapNot nots

{- | Contains all the callbacks to use for initialized the language server.
 it is parameterized over a config type variable representing the type for the
 specific configuration data the language server needs to use.
-}
data ServerDefinition r k = forall m.
  ServerDefinition
  { initialHandlers :: Handlers r k m
  , mainAction :: Maybe (m ())
  , interpret :: ServerEnv r k IO -> (m <~> IO)
  }

initServer :: (Method k) => RPC.RpcEnv r k (ResponseCallback r k IO) -> ServerDefinition r k -> IO (ServerEnv r k IO, Maybe (IO ()))
initServer rpcEnv ServerDefinition{initialHandlers, interpret, mainAction} = do
  handlersVar <- newTVarIO mempty
  let env = ServerEnv{resHandlers = handlersVar, resRpcEnv = rpcEnv}
      interpreter = interpret env
      handlers = transmuteHandlers interpreter initialHandlers
      main = forward interpreter <$> mainAction
  atomically $ writeTVar handlersVar handlers
  pure (env, main)

{- | Add a request handler for the given method.

 Returns 'False' if the method already has a handler registered. In this case the handler
 will be replaced only if the 'force' argument is 'True'.
-}
addRequestHandler ::
  forall r k meth f m.
  (MonadServer r k f m, Method k) =>
  Bool ->
  Sing meth ->
  RequestHandler r k f meth ->
  m Bool
addRequestHandler force s cb = do
  env <- getServerEnv
  liftIO $ atomically $ stateTVar (resHandlers env) $ \old ->
    let new = old{reqHandlers = DMap.insert s cb (reqHandlers old)}
     in case DMap.lookup s (reqHandlers old) of
          Just _ -> (False, if force then new else old)
          Nothing -> (True, new)

{- | Add a notification handler for the given method.

 Returns 'False' if the method already has a handler registered. In this case the handler
 will be replaced only if the 'force' argument is 'True'.
-}
addNotificationHandler ::
  forall r k meth f m .
  (MonadServer r k f m, Method k) =>
  Bool ->
  Sing meth ->
  NotificationHandler r k f meth ->
  m Bool
addNotificationHandler force s cb = do
  env <- getServerEnv
  liftIO $ atomically $ stateTVar (resHandlers env) $ \old ->
    let new = old{notHandlers = DMap.insert s cb (notHandlers old)}
     in case DMap.lookup s (notHandlers old) of
          Just _ -> (False, if force then new else old)
          Nothing -> (True, new)

-- | Send a request and wait synchronously for the response.
request ::
  forall r k (meth :: k) f m .
  (RPC.MonadRpc r k (ResponseCallback r k f) m, Method k, RequestOk r meth, MonadIO f) =>
  Sing meth ->
  MessageParams meth ->
  m (Either (ResponseError meth) (MessageResult meth))
request m p = do
  done <- liftIO newEmptyMVar
  void $ RPC.sendRequest m p $ ResponseCallback (liftIO . putMVar done)
  liftIO $ takeMVar done
