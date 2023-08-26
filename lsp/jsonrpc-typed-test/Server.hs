{-# LANGUAGE EmptyCase #-}
module Server where

import JSONRPC.Server.Control
import JSONRPC.Server.Core
import JSONRPC.Types.Method
import JSONRPC.Types.Message
import JSONRPC.Session qualified as Session
import JSONRPC.RPC qualified as RPC

import TestLib
import Control.Concurrent
import Colog.Core as L
import Control.Concurrent.STM
import Data.ByteString
import Prettyprinter
import Data.Singletons
import Control.Lens hiding (Iso)
import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.IO.Class
import System.Console.Concurrent
import qualified Control.Concurrent.Async as Async
import Control.Exception
import Data.Either
import Data.Functor (void)
import Test.QuickCheck
import Data.Foldable
import Prelude hiding (id)

adder :: ServerDefinition Server TestMethod
adder = ServerDefinition
  { initialHandlers =
    requestHandler SM1 (RequestHandler $ \msg -> pure $ Right ((msg ^. params) + 1) )
  , mainAction = Nothing
  , interpret = \env -> Iso (runServerT env) liftIO
  }

adderPlusNotify :: ServerDefinition Server TestMethod
adderPlusNotify = ServerDefinition
  { initialHandlers =
    requestHandler SM1 $ RequestHandler $ \msg -> do
      RPC.sendNotification SM4 1
      pure $ Right ((msg ^. params) + 1)
  , mainAction = Nothing
  , interpret = \env -> Iso (runServerT env) liftIO
  }

adderPlusNewHandler :: ServerDefinition Server TestMethod
adderPlusNewHandler = ServerDefinition
  { initialHandlers =
    requestHandler SM1 $ RequestHandler $ \msg -> do
      _ <- addRequestHandler False SM5 $ RequestHandler $ \msg -> do
        pure $ Right ((msg ^. params) - 1)
      pure $ Right ((msg ^. params) + 1)
  , mainAction = Nothing
  , interpret = \env -> Iso (runServerT env) liftIO
  }

askNotify :: IO (ServerDefinition Server TestMethod)
askNotify = do
  var <- newMVar 0
  pure $ ServerDefinition
    { initialHandlers =
      (requestHandler SM1 $ RequestHandler $ \_ -> do
        n <- liftIO $ readMVar var
        pure $ Right n)
      <>
      (notificationHandler SM2 $ NotificationHandler $ \msg -> do
        void $ liftIO $ swapMVar var (msg ^. params))
    , mainAction = Nothing
    , interpret = \env -> Iso (runServerT env) liftIO
    }

runServerClientPair :: forall k . Method k => ServerDefinition Server k -> Session.Session Client k () -> IO ()
runServerClientPair server client = do
  (var, logM) <- logMVar
  let
    prettyMsg :: Pretty msg => Role -> WithSeverity msg -> Doc ann
    prettyMsg r l = brackets (viaShow r) <+> brackets (viaShow (L.getSeverity l)) <+> pretty (L.getMsg l)
    loggerFor :: Pretty msg => SRole r -> LogAction IO (WithSeverity msg)
    loggerFor r = L.cmap (\msg -> prettyMsg (fromSing r) msg) logM

  serverOut <- atomically newTChan :: IO (TChan ByteString)
  serverIn <- atomically newTChan :: IO (TChan ByteString)
  let clientIn = serverOut
      clientOut = serverIn

  srpc <-
    withSingI SServer $
      RPC.initRpcEnvWith
        (loggerFor SServer)
        (atomically $ readTChan serverIn)
        (atomically . writeTChan serverOut)
  crpc <-
    withSingI SClient $
      RPC.initRpcEnvWith
        (loggerFor SClient)
        (atomically $ readTChan clientIn)
        (atomically . writeTChan clientOut)

  runServerIn (loggerFor SServer) srpc server `Async.race_` Session.runSessionIn (loggerFor SClient) crpc client
    --`finally`
    --printMVarLogs var

test_requestResponse :: TestTree
test_requestResponse = testCase "requestResponse" $ withConcurrentOutput $
  runServerClientPair adder $ withSingI SClient $ do
    do
      resp <- Session.request SM1 1
      liftIO $ resp @?= Right 2

    do
      resp <- Session.request SM1 3
      liftIO $ resp @?= Right 4

test_notifications :: TestTree
test_notifications = testCase "notifications" $ withConcurrentOutput $ do
  s <- askNotify
  runServerClientPair s $ withSingI SClient $ do
    do
      resp <- Session.request SM1 1
      liftIO $ resp @?= Right 0

    RPC.sendNotification SM2 1

    do
      resp <- Session.request SM1 1
      liftIO $ resp @?= Right 1

    RPC.sendNotification SM2 3

    do
      resp <- Session.request SM1 1
      liftIO $ resp @?= Right 3

test_interveningNotification :: TestTree
test_interveningNotification = testCase "interveningNotification" $ withConcurrentOutput $
  runServerClientPair adderPlusNotify $ do
    id1 <- RPC.sendRequest SM1 1 mempty
    notif <- Session.expectNotification SM4
    liftIO $ notif ^. params @?= 1
    resp <- Session.expectResponseFor SM1 id1
    liftIO $ resp ^. result @?= Right 2

prop_spam :: Property
prop_spam = withSingI SClient $ noShrinking $ property $ \(msgs :: [SomeMessage Client TestMethod]) -> ioProperty $ do
  runServerClientPair adder $ do
    for_ @_ @_ @_ @() msgs $ \case
      (SomeMessage (Req s msg)) -> do
        -- Don't just send the message, it will have a
        -- generated ID and we won't recognize the response
        -- when it comes back!
        rid <- RPC.sendRequest s (msg ^. params) mempty
        void $ Session.expectResponseFor s rid

      (SomeMessage (Not s msg)) ->
        RPC.sendNotification s (msg ^. params)

      (SomeMessage (Rsp s msg)) ->
        RPC.sendResponse s (msg ^. id) (msg ^. result)

    -- Server should still be alive and respond properly
    do
      resp <- Session.request SM1 1
      liftIO $ resp @?= Right 2

test_newHandler :: TestTree
test_newHandler = testCase "newHandler" $ withConcurrentOutput $
  runServerClientPair adderPlusNewHandler $ withSingI SClient $ do
    do
      resp <- Session.request SM5 3
      liftIO $ assertBool "Request unexpectedly succeeded" $ isLeft resp

    do
      resp <- Session.request SM1 1
      liftIO $ resp @?= Right 2

    do
      resp <- Session.request SM5 3
      liftIO $ resp @?= Right 2

