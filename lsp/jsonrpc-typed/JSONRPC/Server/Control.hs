module JSONRPC.Server.Control where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), (<&))
import Colog.Core qualified as L
import Prettyprinter
import System.IO
import JSONRPC.Server.Core
import JSONRPC.IO qualified as IO
import JSONRPC.Server.Processing qualified as Processing
import JSONRPC.Types.Method
import JSONRPC.RPC qualified as RPC
import Data.Singletons
import qualified Control.Concurrent.Async as Async
import Colog.Core.Action (cmap)
import Control.Lens
import Control.Applicative
import Control.Monad.Catch

data ServerLog r k
  = ProcessingLog (Processing.ProcessingLog r k)
  | IoLog IO.IoLog
  | Starting
  | Stopping
  | ExceptionalStop SomeException
  deriving (Show)

instance Method k => Pretty (ServerLog r k) where
  pretty (ProcessingLog l) = pretty l
  pretty (IoLog l) = pretty l
  pretty Starting = "Starting server"
  pretty Stopping = "Stopping server"
  pretty (ExceptionalStop e) = "Stopping triggered by exception:" <+> pretty (displayException e)

-- ---------------------------------------------------------------------

{- | Convenience function for 'runServerWithHandles' which:
     (1) reads from stdin;
     (2) writes to stdout; and
     (3) logs to stderr
-}
runServer :: forall r k. (Method k) => Sing r -> ServerDefinition r k -> IO ()
runServer rsing serverDefinition = do
  rpc <- withSingI rsing $ RPC.initRpcEnvWithHandles (cmap (fmap IoLog) logger) stdin stdout
  runServerIn logger rpc serverDefinition
 where
  prettyMsg l = brackets (viaShow (L.getSeverity l)) <+> pretty (L.getMsg l)
  logger :: LogAction IO (WithSeverity (ServerLog r k))
  logger = L.cmap (show . prettyMsg) L.logStringStderr

runServerIn ::
  forall r k .
  (Method k) =>
  LogAction IO (WithSeverity (ServerLog r k)) ->
  (RPC.RpcEnv r k (ResponseCallback r k IO), Async.Concurrently ()) ->
  ServerDefinition r k ->
  IO ()
runServerIn logger (rpcEnv, rpcThreads) serverDefinition = do
  (env, main) <- initServer rpcEnv serverDefinition
  let mact = case main of
        Just m -> Async.Concurrently m
        Nothing -> empty

  let processing =
        Processing.processingLoop
          (cmap (fmap ProcessingLog) logger)
          env
          (rpcEnv ^. RPC.conn)

  -- Bind all the threads together so that any of them terminating will terminate everything
  bracket_
    (logger <& Starting `WithSeverity` Info)
    (logger <& Stopping `WithSeverity` Info)
    $ (Async.runConcurrently $ mact <|> rpcThreads <|> processing)
      `catch`
      (\(e :: SomeException) -> logger <& ExceptionalStop e `WithSeverity` Info >> throwM e)
