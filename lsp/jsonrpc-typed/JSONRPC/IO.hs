module JSONRPC.IO (recvThread, sendThread, IoLog) where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), (<&))
import Control.Applicative ((<|>))
import Control.Monad
import Data.Attoparsec.ByteString qualified as Attoparsec
import Data.Attoparsec.ByteString.Char8
import Data.ByteString qualified as BS
import Data.List
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Prettyprinter
import Control.Exception (bracket_)
import qualified Control.Concurrent.Async as Async

data IoLog
  = HeaderParseFail [String] String
  | BodyParseFail String
  | RecvMsg BS.ByteString
  | SendMsg BS.ByteString
  | StartingSend
  | StoppingSend
  | StartingRecv
  | StoppingRecv
  | EOF
  deriving (Show)

instance Pretty IoLog where
  pretty (HeaderParseFail ctxs err) =
    vsep
      [ "Failed to parse message header:"
      , pretty (intercalate " > " ctxs) <> ": " <+> pretty err
      ]
  pretty (BodyParseFail err) =
    vsep
      [ "Failed to parse message body:"
      , pretty err
      ]
  pretty (RecvMsg msg) = vsep [ "---> ", pretty (T.decodeUtf8 msg) ]
  pretty (SendMsg msg) = vsep [ "<--- ", pretty (T.decodeUtf8 msg) ]
  pretty StartingSend = "Starting send thread"
  pretty StoppingSend = "Stopping send thread"
  pretty StartingRecv = "Starting receive thread"
  pretty StoppingRecv = "Stopping receive thread"
  pretty EOF = "Got EOF"

-- | Thread which receives JSONRPC messages, parses them, and sends them on.
recvThread ::
  forall msg .
  LogAction IO (WithSeverity IoLog) ->
  -- | Message parsing function.
  (BS.ByteString -> IO (Either String msg)) ->
  -- | Action to pull in new messages (e.g. from a handle).
  IO BS.ByteString ->
  -- | Action to send out messages on.
  (msg -> IO ()) ->
  Async.Concurrently ()
recvThread logger parseMsg clientIn msgOut =
  Async.Concurrently $ bracket_
    (logger <& StartingRecv `WithSeverity` Debug)
    (logger <& StoppingRecv `WithSeverity` Debug)
    $ do
      bs <- clientIn
      loop (parse parser bs)
 where
  loop :: Result BS.ByteString -> IO ()
  loop (Fail _ ctxs err) = do
    logger <& HeaderParseFail ctxs err `WithSeverity` Error
    -- exit
    pure ()
  loop (Partial c) = do
    bs <- clientIn
    if BS.null bs
      then do
        logger <& EOF `WithSeverity` Error
        -- exit
        pure ()
      else loop (c bs)
  loop (Done remainder parsed) = do
    pmsg <- parseMsg parsed
    -- TODO: figure out how to re-enable
    --logger <& RecvMsg parsed `WithSeverity` Debug
    case pmsg of
      -- Note: this is recoverable, because we can just discard the
      -- message and keep going, whereas a header parse failure is
      -- not recoverable
      Left err -> logger <& BodyParseFail err `WithSeverity` Error
      Right msg -> msgOut msg
    loop (parse parser remainder)

  parser = do
    try contentType <|> return ()
    len <- contentLength
    try contentType <|> return ()
    _ <- string _ONE_CRLF
    Attoparsec.take len

  contentLength = do
    _ <- string "Content-Length: "
    len <- decimal
    _ <- string _ONE_CRLF
    return len

  contentType = do
    _ <- string "Content-Type: "
    skipWhile (/= '\r')
    _ <- string _ONE_CRLF
    return ()

-- | Process which receives protocol messages and sends them as JSONRPC messages.
sendThread ::
  forall msg .
  LogAction IO (WithSeverity IoLog) ->
  -- | Message serialisation function.
  (msg -> BS.ByteString) ->
  -- | Action to receive messages.
  IO msg ->
  -- | Action to send serialised messages.
  (BS.ByteString -> IO ()) ->
  Async.Concurrently ()
sendThread logger serialiseMsg msgIn clientOut =
  Async.Concurrently $ bracket_
    (logger <& StartingSend `WithSeverity` Debug)
    (logger <& StoppingSend `WithSeverity` Debug)
    $ forever $ do
      msg <- msgIn

      -- We need to make sure we only send over the content of the message,
      -- and no other tags/wrapper stuff
      let str = serialiseMsg msg

      let out =
            BS.concat
              [ T.encodeUtf8 $ T.pack $ "Content-Length: " ++ show (BS.length str)
              , _TWO_CRLF
              , str
              ]

      -- TODO: figure out how to re-enable
      --logger <& SendMsg out `WithSeverity` Debug
      clientOut out

_ONE_CRLF :: BS.ByteString
_ONE_CRLF = "\r\n"
_TWO_CRLF :: BS.ByteString
_TWO_CRLF = "\r\n\r\n"
