{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module TestLib where

import Colog.Core
import JSONRPC.Types.Method
import Data.Singletons
import Data.GADT.Compare
import Data.GADT.Compare.TH
import Data.Aeson
import GHC.Generics (Generic)
import Data.Void
import Control.Monad.IO.Class
import Control.Concurrent
import System.Console.Concurrent
import Data.Traversable
import Data.Foldable
import Data.Singletons.TH qualified as TH
import Test.QuickCheck
import JSONRPC.Types.Message
import JSONRPC.Types.Id
import qualified Data.Text as T
import Data.String
import Data.Maybe

logStderrConcurrent :: MonadIO m => LogAction m String
logStderrConcurrent = LogAction $ \s -> liftIO $ errorConcurrent s

logMVar :: MonadIO m => m (MVar [a], LogAction m a)
logMVar = liftIO $ do
  var <- newMVar []
  pure (var, LogAction $ \s -> liftIO $ modifyMVar_ var (\logs -> pure (s:logs)))

printMVarLogs :: (Show a, MonadIO m) => MVar [a] -> m ()
printMVarLogs v = liftIO $ do
  logs <- readMVar v
  for_ (reverse logs) print

TH.genDefunSymbols [''Allowed, ''MsgKind, ''Initiator]
TH.singletons [d|
  data TestMethod = M1 | M2 | M3 | M4 | M5

  tmMsgInitiator :: TestMethod -> Initiator
  tmMsgInitiator M1 = ClientInitiates
  tmMsgInitiator M2 = ClientInitiates
  tmMsgInitiator M3 = ServerInitiates
  tmMsgInitiator M4 = ServerInitiates
  tmMsgInitiator M5 = ClientInitiates

  tmMsgKind :: TestMethod -> MsgKind
  tmMsgKind M1 = Request
  tmMsgKind M2 = Notification
  tmMsgKind M3 = Request
  tmMsgKind M4 = Notification
  tmMsgKind M5 = Request
  |]

deriving stock instance Show TestMethod
deriving stock instance Eq TestMethod
deriving stock instance Generic TestMethod
deriving stock instance Enum TestMethod
deriving stock instance Bounded TestMethod
deriving anyclass instance ToJSON TestMethod
deriving anyclass instance FromJSON TestMethod

deriving stock instance Eq (STestMethod m)
deriving stock instance Show (STestMethod m)

deriveGEq ''STestMethod
deriveGCompare ''STestMethod

instance Closed TestMethod where
  type Elements TestMethod = [M1, M2, M3, M4, M5]

  bring _ s k = case s of
    SM1 -> k
    SM2 -> k
    SM3 -> k
    SM4 -> k
    SM5 -> k

-------------------------

instance Method TestMethod where
  type MessageInitiator m = TmMsgInitiator m
  sMessageInitiator = sTmMsgInitiator


  type MessageKind m = TmMsgKind m
  sMsgKind = sTmMsgKind

  type MessageParams M1 = Integer
  type MessageParams M2 = Integer
  type MessageParams M3 = Integer
  type MessageParams M4 = Integer
  type MessageParams M5 = Integer

  type MessageResult M1 = Integer
  type MessageResult M2 = ()
  type MessageResult M3 = Integer
  type MessageResult M4 = ()
  type MessageResult M5 = Integer

  type ErrorData M1 = ()
  type ErrorData M2 = ()
  type ErrorData M3 = ()
  type ErrorData M4 = ()
  type ErrorData M5 = ()

instance Arbitrary Id where
  arbitrary = oneof [ (IdInt <$> arbitrary),  (IdString . T.pack <$> arbitrary)]

class (Arbitrary (MessageParams m), Arbitrary (MessageResult m), Arbitrary (ErrorData m)) => ArbitraryParts m
instance (Arbitrary (MessageParams m), Arbitrary (MessageResult m), Arbitrary (ErrorData m)) => ArbitraryParts m

instance (Closed k, k `Everywhere` ArbitraryParts, SingI m) => Arbitrary (RequestMessage (m :: k)) where
  arbitrary =
    bring @_ @ArbitraryParts (Proxy @k) (sing @m) $
      RequestMessage "2.0" <$> arbitrary <*> pure (sing @m) <*> arbitrary

instance (Closed k, k `Everywhere` ArbitraryParts, SingI m) => Arbitrary (NotificationMessage (m :: k)) where
  arbitrary =
    bring @_ @ArbitraryParts (Proxy @k) (sing @m) $
      NotificationMessage "2.0" <$> pure (sing @m) <*> arbitrary

instance (Closed k, k `Everywhere` ArbitraryParts, SingI m) => Arbitrary (ResponseError (m :: k)) where
  arbitrary =
    bring @_ @ArbitraryParts (Proxy @k) (sing @m) $
      ResponseError <$> arbitrary <*> (fromString <$> arbitrary) <*> arbitrary

instance (Closed k, k `Everywhere` ArbitraryParts, SingI m) => Arbitrary (ResponseMessage (m :: k)) where
  arbitrary =
    bring @_ @ArbitraryParts (Proxy @k) (sing @m) $
      ResponseMessage "2.0" <$> arbitrary <*> arbitrary

instance (Method k, Closed k, k `Everywhere` ArbitraryParts, SingI r, Enum k, Bounded k) => Arbitrary (SomeMessage r k) where
  arbitrary = do
    let ms :: [k] = [minBound .. maxBound]
    let gs :: [Gen (SomeMessage r k)] = mapMaybe genMsgFor ms
    oneof gs
    where
      genMsgFor :: k -> Maybe (Gen (SomeMessage r k))
      genMsgFor m =
        case toSing m of
          SomeSing sng -> withSingI sng $
            let
              rsng = sing @r
              orsng = sOtherRole rsng
              is = sMessageInitiator sng
            in case (sMsgKind sng, sCanInitiate rsng is, sCanInitiate orsng is) of
              (SRequest, SYes, _) -> Just $ SomeMessage . Req sng <$> arbitrary
              (SNotification, SYes, _) -> Just $ SomeMessage . Not sng <$> arbitrary
              (SRequest, _, SYes) -> Just $ SomeMessage . Rsp sng <$> arbitrary
              _ -> Nothing
