{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Colog.Core qualified as L
import Control.Applicative.Combinators
import Control.Exception
import Control.Lens hiding (Iso, List)
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Language.LSP.Protocol.Lens qualified as L
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.Test qualified as Test
import System.Exit
import System.IO
import System.Process
import Test.Hspec
import UnliftIO
import UnliftIO.Concurrent

main :: IO ()
main = hspec $ do
  let logger = L.cmap show L.logStringStderr
  describe "progress reporting" $
    it "sends end notification if thread is killed" $ do
      (hinRead, hinWrite) <- createPipe
      (houtRead, houtWrite) <- createPipe

      killVar <- newEmptyMVar

      let definition =
            ServerDefinition
              { parseConfig = const $ const $ Right ()
              , onConfigChange = const $ pure ()
              , defaultConfig = ()
              , configSection = "demo"
              , doInitialize = \env _req -> pure $ Right env
              , staticHandlers = \_caps -> handlers killVar
              , interpretHandler = \env -> Iso (runLspT env) liftIO
              , options = defaultOptions
              }

          handlers :: MVar () -> Handlers (LspM ())
          handlers killVar =
            notificationHandler SMethod_Initialized $ \noti -> do
              tid <- withRunInIO $ \runInIO ->
                forkIO $
                  runInIO $
                    withProgress "Doing something" Nothing NotCancellable $ \updater -> do
                      -- Wait around to be killed
                      liftIO $ threadDelay (1 * 1000000)
              liftIO $ void $ forkIO $ do
                takeMVar killVar
                killThread tid

      forkIO $ void $ runServerWithHandles logger (L.hoistLogAction liftIO logger) hinRead houtWrite definition

      Test.runSessionWithHandles hinWrite houtRead Test.defaultConfig Test.fullCaps "." $ do
        -- First make sure that we get a $/progress begin notification
        skipManyTill Test.anyMessage $ do
          x <- Test.message SMethod_Progress
          guard $ has (L.params . L.value . _workDoneProgressBegin) x

        -- Then kill the thread
        liftIO $ putMVar killVar ()

        -- Then make sure we still get a $/progress end notification
        skipManyTill Test.anyMessage $ do
          x <- Test.message SMethod_Progress
          guard $ has (L.params . L.value . _workDoneProgressEnd) x

  describe "workspace folders" $
    it "keeps track of open workspace folders" $ do
      (hinRead, hinWrite) <- createPipe
      (houtRead, houtWrite) <- createPipe

      countVar <- newMVar 0

      let wf0 = WorkspaceFolder (filePathToUri "one") "Starter workspace"
          wf1 = WorkspaceFolder (filePathToUri "/foo/bar") "My workspace"
          wf2 = WorkspaceFolder (filePathToUri "/foo/baz") "My other workspace"

          definition =
            ServerDefinition
              { parseConfig = const $ const $ Right ()
              , onConfigChange = const $ pure ()
              , defaultConfig = ()
              , configSection = "demo"
              , doInitialize = \env _req -> pure $ Right env
              , staticHandlers = \_caps -> handlers
              , interpretHandler = \env -> Iso (runLspT env) liftIO
              , options = defaultOptions
              }

          handlers :: Handlers (LspM ())
          handlers =
            mconcat
              [ notificationHandler SMethod_Initialized $ \noti -> do
                  wfs <- fromJust <$> getWorkspaceFolders
                  liftIO $ wfs `shouldContain` [wf0]
              , notificationHandler SMethod_WorkspaceDidChangeWorkspaceFolders $ \noti -> do
                  i <- liftIO $ modifyMVar countVar (\i -> pure (i + 1, i))
                  wfs <- fromJust <$> getWorkspaceFolders
                  liftIO $ case i of
                    0 -> do
                      wfs `shouldContain` [wf1]
                      wfs `shouldContain` [wf0]
                    1 -> do
                      wfs `shouldNotContain` [wf1]
                      wfs `shouldContain` [wf0]
                      wfs `shouldContain` [wf2]
                    _ -> error "Shouldn't be here"
              ]

      server <- async $ void $ runServerWithHandles logger (L.hoistLogAction liftIO logger) hinRead houtWrite definition

      let config =
            Test.defaultConfig
              { Test.initialWorkspaceFolders = Just [wf0]
              }

          changeFolders add rmv =
            let ev = WorkspaceFoldersChangeEvent add rmv
                ps = DidChangeWorkspaceFoldersParams ev
             in Test.sendNotification SMethod_WorkspaceDidChangeWorkspaceFolders ps

      Test.runSessionWithHandles hinWrite houtRead config Test.fullCaps "." $ do
        changeFolders [wf1] []
        changeFolders [wf2] [wf1]

      Left e <- waitCatch server
      fromException e `shouldBe` Just ExitSuccess
