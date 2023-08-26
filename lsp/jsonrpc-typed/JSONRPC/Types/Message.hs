{-# LANGUAGE TemplateHaskell #-}

module JSONRPC.Types.Message where

import Language.LSP.Protocol.Utils.Misc

import JSONRPC.Types.Id
import JSONRPC.Types.Method

import Control.Lens hiding ((.=))

import Data.Aeson hiding (Null)
import Data.Aeson qualified as J
import Data.Aeson.Types qualified as J
import Data.Int (Int32)
import Data.Singletons
import Data.String (IsString (..))
import Data.Text (Text)
import GHC.Generics
import Prettyprinter

-- | Typed notification message, containing the correct parameter payload.
data NotificationMessage (m :: k) = NotificationMessage
  { _jsonrpc :: Text
  , _method :: Sing m
  , _params :: MessageParams m
  }
  deriving stock (Generic)

deriving stock instance (Eq (Sing m), Eq (MessageParams m)) => Eq (NotificationMessage m)
deriving stock instance (Show (Sing m), Show (MessageParams m)) => Show (NotificationMessage m)

{- Note [Missing 'params']
The 'params' field on requrests and notificaoins may be omitted according to the
JSON-RPC spec, but that doesn't quite work the way we want with the generic aeson
instance. Even if the 'MessageParams' type family happens to resolve to a 'Maybe',
we handle it generically and so we end up asserting that it must be present.

We fix this in a slightly dumb way by just adding the field in if it is missing,
set to null (which parses correctly for those 'Maybe' parameters also).
-}

instance (Method k, SingI m) => FromJSON (NotificationMessage (m :: k)) where
  -- See Note [Missing 'params']
  parseJSON v = f (addNullField "params" v)
    where
      -- TODO: check it
      implicitSing = sing @m
      f = bring @_ @IsJsonParts (Proxy @k) implicitSing $
            withObject "Notification message" $ \o ->
              NotificationMessage <$> o .: "jsonrpc" <*> pure implicitSing <*> o .: "params"

instance (Method k) => ToJSON (NotificationMessage (m :: k)) where
  toJSON (NotificationMessage jsonrpc sng params) =
    withSingI sng $
      bring @_ @IsJsonParts (Proxy @k) sng $
        object ["jsonrpc" .= jsonrpc, "method" .= fromSing sng, "params" .= params]

-- deriving via ViaJSON (NotificationMessage m) instance (Method k, SingI (m :: k)) => Pretty (NotificationMessage m)

-- | Typed request message, containing the correct parameter payload.
data RequestMessage (m :: k) = RequestMessage
  { _jsonrpc :: Text
  , _id :: Id
  , _method :: Sing m
  , _params :: MessageParams m
  }
  deriving stock (Generic)

deriving stock instance (Eq (Sing m), Eq (MessageParams m)) => Eq (RequestMessage m)
deriving stock instance (Show (Sing m), Show (MessageParams m)) => Show (RequestMessage m)

instance (Method k, SingI m) => FromJSON (RequestMessage (m :: k)) where
  -- See Note [Missing 'params']
  parseJSON v = f (addNullField "params" v)
    where
      implicitSing = sing @m
      f = bring @_ @IsJsonParts (Proxy @k) implicitSing $
            withObject "Request message" $ \o ->
              RequestMessage <$> o .: "jsonrpc" <*> o .: "id" <*> pure implicitSing <*> o .: "params"

instance (Method k) => ToJSON (RequestMessage (m :: k)) where
  toJSON (RequestMessage jsonrpc i sng params) =
    withSingI sng $
      bring @_ @IsJsonParts (Proxy @k) sng $
        object ["jsonrpc" .= jsonrpc, "id" .= i, "method" .= fromSing sng, "params" .= params]

-- deriving via ViaJSON (RequestMessage m) instance (Method k, SingI (m :: k)) => Pretty (RequestMessage m)

data ResponseError (m :: k) = ResponseError
  { _code :: Int32
  , _message :: Text
  , _xdata :: Maybe (ErrorData m)
  }
  deriving stock (Generic)

deriving stock instance Eq (ErrorData m) => Eq (ResponseError m)
deriving stock instance Show (ErrorData m) => Show (ResponseError m)

instance (Method k, SingI m) => FromJSON (ResponseError (m :: k)) where
  parseJSON =
    bring @_ @IsJsonParts (Proxy @k) (sing @m) $
      withObject "ResponseError" $ \v ->
        ResponseError
            <$> v .: "code"
            <*> v .: "message"
            <*> v .:? "data"
instance (Method k, SingI m) => ToJSON (ResponseError (m :: k)) where
  toJSON (ResponseError code message xdata) =
    bring @_ @IsJsonParts (Proxy @k) (sing @m) $
      object ["code" .= code, "message" .= message, "data" .= xdata]

-- deriving via ViaJSON (ResponseError m) instance (Method k, SingI (m :: k)) => Pretty (ResponseError m)

-- | A typed response message with a correct result payload.
data ResponseMessage (m :: k) = ResponseMessage
  { _jsonrpc :: Text
  , _id :: Id
  , _result :: Either (ResponseError m) (MessageResult m)
  }
  deriving stock (Generic)

deriving stock instance (Eq (MessageResult m), Eq (ErrorData m)) => Eq (ResponseMessage m)
deriving stock instance (Show (MessageResult m), Show (ErrorData m)) => Show (ResponseMessage m)

instance (Method k, SingI m) => ToJSON (ResponseMessage (m :: k)) where
  toJSON ResponseMessage{_jsonrpc = jsonrpc, _id = lspid, _result = result} =
    bring @_ @IsJsonParts (Proxy @k) (sing @m) $
      object
        [ "jsonrpc" .= jsonrpc
        , "id" .= lspid
        , case result of
            Left err -> "error" .= err
            Right a -> "result" .= a
        ]

instance (Method k, SingI m) => FromJSON (ResponseMessage (m :: k)) where
  parseJSON =
    bring @_ @IsJsonParts (Proxy @k) (sing @m) $
      withObject "Response" $ \o -> do
        _jsonrpc <- o .: "jsonrpc"
        _id <- o .: "id"
        -- It is important to use .:! so that "result = null" (without error) gets decoded as Just Null
        _result <- o .:! "result"
        _error <- o .:? "error"
        result <- case (_error, _result) of
          (Just err, Nothing) -> pure $ Left err
          (Nothing, Just res) -> pure $ Right res
          (Just _err, Just _res) -> fail $ "both error and result cannot be present: " ++ show o
          (Nothing, Nothing) -> fail "both error and result cannot be Nothing"
        return $ ResponseMessage _jsonrpc _id result

-- deriving via ViaJSON (ResponseMessage m) instance (Method k, SingI (m :: k)) => Pretty (ResponseMessage m)

{- | Look at a message encoded as a 'Value' and try to determine the method associated with it.

 This is important because in order to parse the message we need to know the method singleton.
 It is not entirely trivial, unfortunately, because not all messages have method fields.
 Responses do not, and just have an id field. So we also need a way to check the method associated
 with an id.
-}
probeMethod :: forall k. (Method k) => (Id -> Maybe (SomeSing k)) -> Value -> J.Parser (SomeSing k)
probeMethod idLookup = withObject "Message" $ \o -> do
  (methM :: Maybe k) <- o .:! "method"
  (idM :: Maybe Id) <- o .:! "id"
  case (methM, idM) of
    (Just m, _) -> pure $ toSing m
    (_, Just i) -> case idLookup i of
      Just m -> pure m
      Nothing -> fail $ "Id " ++ show i ++ " is not known, cannot determine method"
    _ -> fail "Message has neither method nor id fields, cannot determine method"

-- ---------------------------------------------------------------------
-- Combined message types
-- ---------------------------------------------------------------------

data Message r m where
  Not :: (NotificationOk r m) => Sing m -> NotificationMessage m -> Message r m
  Req :: (RequestOk r m) => Sing m -> RequestMessage m -> Message r m
  Rsp :: (ResponseOk r m) => Sing m -> ResponseMessage m -> Message r m

deriving stock instance (Eq (Sing m), Eq (MessageParams m), Eq (MessageResult m), Eq (ErrorData m)) => Eq (Message r m)
deriving stock instance (Show (Sing m), Show (MessageParams m), Show (MessageResult m), Show (ErrorData m)) => Show (Message r m)

instance (Method k, SingI m, SingI r) => FromJSON (Message r (m :: k)) where
  parseJSON = withObject "Message" $ \o -> do
    let ms = sing @m
        rs = sing @r
        ors = sOtherRole rs
        is = sMessageInitiator ms
    case (sMsgKind ms, sCanInitiate rs is, sCanInitiate ors is) of
        (SNotification, SYes, _) -> Not ms <$> parseJSON (Object o)
        (SRequest, SYes, _) -> Req ms <$> parseJSON (Object o)
        (SRequest, _, SYes) -> Rsp ms <$> parseJSON (Object o)
        -- TODO: more error
        _ -> fail $ "got message that can't be sent: " ++ show (fromSing ms)

instance (Method k) => ToJSON (Message r (m :: k)) where
  toJSON = \case
    Not ms msg -> withSingI ms $ toJSON msg
    Req ms msg -> withSingI ms $ toJSON msg
    Rsp ms msg -> withSingI ms $ toJSON msg

data SomeMessage r k where
  SomeMessage :: forall r k (m :: k) . Message r m -> SomeMessage r k

instance (Method k) => Show (SomeMessage r k) where
  show (SomeMessage msg) = show $ encode msg

instance (Method k) => Pretty (SomeMessage r k) where
  pretty (SomeMessage msg) = prettyJSON $ toJSON msg

parseSomeMessage :: forall r k. (Method k, SingI r) => (Id -> Maybe (SomeSing k)) -> Value -> J.Parser (SomeMessage r k)
parseSomeMessage lookupId v = do
  sm <- probeMethod lookupId v
  case sm of
    SomeSing s -> go s
 where
  go :: forall (m :: k). Sing m -> J.Parser (SomeMessage r k)
  go s = do
    cm <- withSingI s $ parseJSON @(Message r m) v
    pure $ SomeMessage cm

{- | Replace a missing field in an object with a null field, to simplify parsing
 This is a hack to allow other types than Maybe to work like Maybe in allowing the field to be missing.
 See also this issue: https://github.com/haskell/aeson/issues/646
-}
addNullField :: String -> Value -> Value
addNullField s (Object o) = Object $ o <> fromString s .= J.Null
addNullField _ v = v

makeFieldsNoPrefix ''NotificationMessage
makeFieldsNoPrefix ''RequestMessage
makeFieldsNoPrefix ''ResponseMessage
makeFieldsNoPrefix ''ResponseError
