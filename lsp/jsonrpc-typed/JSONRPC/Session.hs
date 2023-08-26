module JSONRPC.Session where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), (<&))
import JSONRPC.RPC qualified as RPC
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Data.Monoid (Ap(..))
import JSONRPC.Types.Message
import Data.Data (Typeable)
import JSONRPC.Types.Method
import Data.Singletons
import Data.Type.Equality
import JSONRPC.Types.Id
import Prelude hiding (id)
import Data.GADT.Compare
import Control.Lens
import qualified Data.Text as T
import Data.String
import qualified Control.Concurrent.Async as Async
import Prettyprinter

data SessionLog =
  Starting
  | Stopping
  | ExceptionalStop SomeException

instance Pretty SessionLog where
  pretty Starting = "Starting session"
  pretty Stopping = "Stopping session"
  pretty (ExceptionalStop e) = "Stopping triggered by exception:" <+> pretty (displayException e)

data SessionEnv r k = SessionEnv
  { _rpcEnv :: RPC.RpcEnv r k (Const ())
  }

newtype SessionT r k m a = Session { runSessionT :: ReaderT (SessionEnv r k) m a}
  deriving newtype (Functor, Applicative, Monad, MonadCatch, MonadIO, MonadMask, MonadThrow, MonadReader (SessionEnv r k), MonadTrans, MonadUnliftIO, MonadFix)
  deriving (Semigroup, Monoid) via (Ap (SessionT r k m) a)

type Session r k a = SessionT r k IO a

class (RPC.MonadRpc r k (Const ()) m, MonadThrow m) => MonadSession r k m | m -> r k where
  getSessionEnv :: m (SessionEnv r k)

instance (MonadThrow m, MonadIO m) => RPC.MonadRpc r k (Const ()) (SessionT r k m) where
  getRpcEnv = asks _rpcEnv

instance (MonadThrow m, MonadIO m) => MonadSession r k (SessionT r k m) where
  getSessionEnv = ask

data UnexpectedMessage r k = UnexpectedMessage T.Text (SomeMessage r k)
  deriving Show

instance (Method k, Typeable k, Typeable r) => Exception (UnexpectedMessage r k)

request ::
  forall r k (meth :: k) m .
  (Typeable k, Typeable r, Typeable (OtherRole r), MonadThrow m,
   Method k, SingI r, RequestOk r meth, MonadSession r k m) =>
  Sing meth ->
  MessageParams meth ->
  m (Either (ResponseError meth) (MessageResult meth))
request ms p = do
  i <- RPC.sendRequest ms p mempty
  (m :: ResponseMessage meth) <- case otherRoleInverse @r of
    Refl -> expectResponseFor ms i
  pure $ m ^. result

expectNotification ::
  forall r k (meth :: k) m .
  (Typeable k, Typeable (OtherRole r), MonadThrow m,
   Method k, NotificationOk (OtherRole r) meth, MonadSession r k m) =>
  Sing meth ->
  m (NotificationMessage meth)
expectNotification ms = do
  m <- RPC.recvMsg
  case m of
    SomeMessage (Not ms' msg) | Just Refl <- geq ms ms' -> pure msg
    msg -> throwM $ UnexpectedMessage "Expected a notification" msg

expectRequest ::
  forall r k (meth :: k) m .
  (Typeable k, Typeable (OtherRole r), MonadThrow m,
   Method k, RequestOk (OtherRole r) meth, MonadSession r k m) =>
  Sing meth ->
  m (RequestMessage meth)
expectRequest ms = do
  m <- RPC.recvMsg
  case m of
    SomeMessage (Req ms' msg) | Just Refl <- geq ms ms' -> pure msg
    msg -> throwM $ UnexpectedMessage "Expected a request" msg

expectResponse ::
  forall r k (meth :: k) m .
  (Typeable k, Typeable (OtherRole r), MonadThrow m,
   Method k, ResponseOk (OtherRole r) meth, MonadSession r k m) =>
  Sing meth ->
  m (ResponseMessage meth)
expectResponse ms = do
  m <- RPC.recvMsg
  case m of
    SomeMessage (Rsp ms' msg) | Just Refl <- geq ms ms' -> pure msg
    msg -> throwM $ UnexpectedMessage "Expected a response" msg

expectResponseFor ::
  forall r k (meth :: k) m .
  (Typeable k, Typeable (OtherRole r), MonadThrow m,
   Method k, ResponseOk (OtherRole r) meth, MonadSession r k m) =>
  Sing meth ->
  Id ->
  m (ResponseMessage meth)
expectResponseFor ms i = do
  msg <- expectResponse ms
  unless (msg ^. id == i) $ throwM $ UnexpectedMessage ("Expected a response with id " <> fromString (show i)) (SomeMessage $ Rsp @(OtherRole r) ms msg)
  pure msg

runSessionIn ::
  forall r k .
  (Method k) =>
  LogAction IO (WithSeverity SessionLog) ->
  (RPC.RpcEnv r k (Const ()), Async.Concurrently ()) ->
  Session r k () ->
  IO ()
runSessionIn logger (rpcEnv, rpcThreads) session = do
  let s = runReaderT (runSessionT session) (SessionEnv rpcEnv)

  -- Bind all the threads together so that any of them terminating will terminate everything
  logger <& Starting `WithSeverity` Info
  (Async.runConcurrently $ Async.Concurrently s <|> rpcThreads)
    `catch`
    (\(e :: SomeException) -> logger <& ExceptionalStop e `WithSeverity` Warning >> throwM e)
  logger <& Stopping `WithSeverity` Info
