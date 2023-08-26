{-# LANGUAGE NamedFieldPuns #-}
module JSONRPC.Server.Processing where

import Colog.Core (
  LogAction (..),
  Severity (..),
  WithSeverity (..),
  (<&),
  hoistLogAction,
 )

import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Except ()
import Control.Monad.IO.Class
import Data.Aeson.Lens ()
import Data.Dependent.Map qualified as DMap
import Data.Dependent.Sum qualified as DSum
import Data.GADT.Compare
import Data.Map.Strict qualified as Map
import Data.Singletons
import Data.Text qualified as T
import Prettyprinter
import JSONRPC.Server.Core
import JSONRPC.Types.Id
import JSONRPC.Types.Message
import JSONRPC.Types.Method
import JSONRPC.RPC qualified as RPC
import Prelude hiding (id)
import Control.Exception (bracket_)

data ProcessingLog r k
  = MissingHandler k
  | MissingResponseHandler k Id [Id]
  | WrongResponseHandlerMethod k Id k
  | HandlingMessage (SomeMessage (OtherRole r) k)
  | SuccessfulRequest (SomeMessage (OtherRole r) k)
  | SuccessfulResponse (SomeMessage (OtherRole r) k)
  | SuccessfulNotification (SomeMessage (OtherRole r) k)
  | StartingProcessing
  | StoppingProcessing
  deriving stock (Show)

instance (Method k) => Pretty (ProcessingLog r k) where
  pretty (MissingHandler m) = "No handler for:" <+> viaShow m
  pretty (MissingResponseHandler _ i ids) = vsep
    [ "No response handler for id:" <+> pretty i
    , "Known ids:" <+> pretty ids
    ]
  pretty (WrongResponseHandlerMethod m1 i m2) =
    "Response handler for id "
      <+> pretty i
      <+> "expected response with method"
      <+> viaShow m1
      <+> "but got"
      <+> viaShow m2
  pretty (HandlingMessage m) = "Handling message:" <+> pretty m
  pretty (SuccessfulRequest m) = "Successfully handled request:" <+> pretty m
  pretty (SuccessfulResponse m) = "Successfully handled response:" <+> pretty m
  pretty (SuccessfulNotification m) = "Successfully handled notification:" <+> pretty m
  pretty StartingProcessing = "Starting processing thread"
  pretty StoppingProcessing = "Stopping processing thread"

processingLoop ::
  forall r k .
  Method k =>
  LogAction IO (WithSeverity (ProcessingLog r k)) ->
  ServerEnv r k IO ->
  RPC.Connection r k ->
  Async.Concurrently ()
processingLoop logger env c =
  Async.Concurrently $ bracket_
    (logger <& StartingProcessing `WithSeverity` Debug)
    (logger <& StoppingProcessing `WithSeverity` Debug)
    $ forever $ do
      msg <- c ^. RPC.recvMessage
      logger <& HandlingMessage msg `WithSeverity` Debug
      action <- atomically $ handleMessage (hoistLogAction liftIO logger) env msg
      runServerT env action

handleMessage ::
  forall r k m.
  (MonadServer r k IO m, Method k) =>
  LogAction m (WithSeverity (ProcessingLog r k)) ->
  ServerEnv r k IO ->
  SomeMessage (OtherRole r) k ->
  STM (m ())
handleMessage logger env smsg@(SomeMessage cmsg) = do
  Handlers{reqHandlers, notHandlers} <- readTVar (resHandlers env)
  let pendingResponsesVar = resRpcEnv env ^. RPC.requestState . RPC.pendingResponses
  pending <- readTVar pendingResponsesVar

  case cmsg of
    Req m msg -> case DMap.lookup m reqHandlers of
      Just (RequestHandler h) -> pure $ do
        res <- liftIO $ h msg
        RPC.sendResponse (msg ^. method) (msg ^. id) res
        logger <& SuccessfulRequest smsg `WithSeverity` Debug
      Nothing -> pure $ do
        let meth = fromSing m
        logger <& MissingHandler meth `WithSeverity` Warning
        let errorMsg = T.pack $ unwords ["No handler for: ", show meth]
            err = ResponseError (-32601) errorMsg Nothing
        RPC.sendResponse (msg ^. method) (msg ^. id) (Left err)
    Not m msg ->
      case DMap.lookup m notHandlers of
        Just (NotificationHandler h) -> pure $ do
          liftIO $ h msg
          logger <& SuccessfulNotification smsg `WithSeverity` Debug
        Nothing -> pure $ logger <& MissingHandler (fromSing m) `WithSeverity` Warning
    Rsp m msg -> case Map.lookup (msg ^. id) pending of
      Just (rm DSum.:=> (ResponseCallback f)) ->
        case geq m rm of
          Just Refl -> do
            let newMap = Map.delete (msg ^. id) pending
            writeTVar pendingResponsesVar newMap
            pure $ do
              liftIO @m $ f (msg ^. result)
              logger <& SuccessfulResponse smsg `WithSeverity` Debug
          -- This shouldn't happen, since we used the resonse map to work out the method to use to parse the
          -- response in the first place
          Nothing -> pure $ logger <& WrongResponseHandlerMethod (fromSing m) (msg ^. id) (fromSing rm) `WithSeverity` Error
      -- This shouldn't happen, since we used the resonse map to work out the method to use to parse the
      -- response in the first place
      Nothing -> pure $ logger <& MissingResponseHandler (fromSing m) (msg ^. id) (Map.keys pending) `WithSeverity` Error
