{-# LANGUAGE TemplateHaskell #-}
module JSONRPC.RPC where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), (<&))
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Data.Default
import Control.Monad.IO.Class
import Data.Dependent.Sum qualified as DSum
import Data.Int (Int32)
import Data.Kind
import Data.Map qualified as Map
import Data.Singletons
import Data.ByteString.Builder.Extra (defaultChunkSize)
import JSONRPC.Types.Id
import JSONRPC.Types.Message
import JSONRPC.Types.Method
import qualified Control.Concurrent.Async as Async
import JSONRPC.IO qualified as IO
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Aeson qualified as J
import Data.Aeson.Types qualified as J
import System.IO

import Prelude hiding (id)
import Control.Applicative

data Connection r k = Connection
  { _sendMessage :: SomeMessage r k -> IO ()
  , _recvMessage :: IO (SomeMessage (OtherRole r) k)
  }

makeFieldsNoPrefix ''Connection

type ResponseMap r k f = Map.Map Id (DSum.DSum (Sing @k) f)

data RequestState r k (f :: k -> Type) = RequestState
  { _pendingResponses :: TVar (ResponseMap r k f)
  , _nextId :: TVar Int32
  }

makeFieldsNoPrefix ''RequestState

initRequestState :: IO (RequestState r k f)
initRequestState = do
  pendingVar <- newTVarIO mempty
  idVar <- newTVarIO 0
  pure $ RequestState {_pendingResponses = pendingVar, _nextId = idVar}

data RpcEnv r k f = RpcEnv
  { _conn :: Connection r k
  , _requestState :: RequestState r k f
  }

makeFieldsNoPrefix ''RpcEnv

data RoleInfo r = RoleInfo {
  _roleSing :: Sing r
  , _otherRoleSing :: Sing (OtherRole r)
  }

makeFieldsNoPrefix ''RoleInfo

instance Default (RoleInfo Server) where
  def = RoleInfo SServer SClient

instance Default (RoleInfo Client) where
  def = RoleInfo SClient SServer

class (Monad m, MonadIO m) => MonadRpc r k f m | m -> r k f where
  getRpcEnv :: m (RpcEnv r k f)

addResponseState ::
  forall r k (meth :: k) m f .
  (Method k, MonadRpc r k f m) =>
  Bool ->
  Id ->
  Sing meth ->
  f meth ->
  m Bool
addResponseState force lid s cb = do
  env <- getRpcEnv
  liftIO $ atomically $ stateTVar (env ^. requestState . pendingResponses) $ \old ->
    let new = Map.insert lid (s DSum.:=> cb) old
     in case Map.lookup lid old of
          Just _ -> (False, if force then new else old)
          Nothing -> (True, new)

sendMsg :: (MonadIO m , MonadRpc r k f m) => SomeMessage r k -> m ()
sendMsg msg = do
  env <- getRpcEnv
  liftIO $ (env ^. conn . sendMessage) msg

recvMsg :: (MonadIO m , MonadRpc r k f m) => m (SomeMessage (OtherRole r) k)
recvMsg = do
  env <- getRpcEnv
  liftIO $ env ^. conn . recvMessage

sendNotification ::
  forall r k (meth :: k) m f .
  (Method k, NotificationOk r meth, MonadRpc r k f m) =>
  Sing meth ->
  MessageParams meth ->
  m ()
sendNotification m p = sendMsg $ SomeMessage $ Not m $ NotificationMessage "2.0" m p

sendResponse ::
  forall r k (meth :: k) f m .
  (Method k, ResponseOk r meth, MonadRpc r k f m) =>
  Sing meth ->
  Id ->
  Either (ResponseError meth) (MessageResult meth) ->
  m ()
sendResponse m i r = sendMsg $ SomeMessage $ Rsp m $ ResponseMessage "2.0" i r

sendRequest ::
  forall r k (meth :: k) f m .
  (Method k, RequestOk r meth, MonadRpc r k f m) =>
  Sing meth ->
  MessageParams meth ->
  f meth ->
  m Id
sendRequest m p resHandler = do
  reqId <- freshId
  success <- addResponseState @r False reqId m resHandler
  -- TODO: this should be impossible, but still better to avoid the error
  unless success $ error $ "Could not send request as id " ++ show reqId ++ " is already used"

  sendMsg $ SomeMessage $ Req m $ RequestMessage "2.0" reqId m p
  return reqId

freshId :: forall r k f m . (MonadRpc r k f m) => m Id
freshId = do
  env <- getRpcEnv
  liftIO $ atomically $ stateTVar (env ^. requestState . nextId) $ \cur ->
    let !next = cur + 1 in (IdInt cur, next)

initRpcEnvWithHandles ::
  forall r k f.
  (Method k, SingI r) =>
  LogAction IO (WithSeverity IO.IoLog) ->
  Handle ->
  Handle ->
  IO (RpcEnv r k f, Async.Concurrently ())
initRpcEnvWithHandles logger hin hout = do
  hSetBuffering hin NoBuffering
  hSetEncoding hin utf8

  hSetBuffering hout NoBuffering
  hSetEncoding hout utf8

  let
    clientIn = BS.hGetSome hin defaultChunkSize

    clientOut out = do
      BS.hPut hout out
      hFlush hout

  initRpcEnvWith logger clientIn clientOut

initRpcEnvWith ::
  forall r k f.
  (Method k, SingI r) =>
  LogAction IO (WithSeverity IO.IoLog) ->
  -- | Input from the client.
  IO BS.ByteString ->
  -- | Server output.
  (BS.ByteString -> IO ()) ->
  IO (RpcEnv r k f, Async.Concurrently ())
initRpcEnvWith logger clientIn clientOut = do
  cout <- atomically newTChan :: IO (TChan (SomeMessage r k))
  cin <- atomically newTChan :: IO (TChan (SomeMessage (OtherRole r) k))

  let sendServerMsg :: SomeMessage r k -> IO ()
      sendServerMsg msg = atomically $ writeTChan cout msg
      recvServerMsg :: IO (SomeMessage r k)
      recvServerMsg = atomically $ readTChan cout

      sendClientMsg :: SomeMessage (OtherRole r) k -> IO ()
      sendClientMsg msg = atomically $ writeTChan cin msg
      recvClientMsg :: IO (SomeMessage (OtherRole r) k)
      recvClientMsg = atomically $ readTChan cin

  let conn = Connection sendServerMsg recvClientMsg
  reqs <- initRequestState
  let rpcEnv = RpcEnv conn reqs

  let
    lookupId :: IO (Id -> Maybe (SomeSing k))
    lookupId = do
      responseMap <- readTVarIO (reqs ^. pendingResponses)
      pure $ \i -> case Map.lookup i responseMap of
        Just (s DSum.:=> _) -> Just (SomeSing s)
        Nothing -> Nothing
    parseMsg msg = withSingI (sOtherRole $ sing @r) $ do
      lf <- lookupId
      pure $ J.eitherDecode (BSL.fromStrict msg) >>= J.parseEither (parseSomeMessage @(OtherRole r) @k lf)
    serialiseMsg (SomeMessage m) = BSL.toStrict $ J.encode m

  let sending =
        IO.sendThread
          logger
          serialiseMsg
          recvServerMsg
          clientOut
      receiving =
        IO.recvThread
          logger
          parseMsg
          clientIn
          sendClientMsg

  -- Bind the threads together so that either of them terminating will terminate both
  let action = sending <|> receiving
  return (rpcEnv, action)
