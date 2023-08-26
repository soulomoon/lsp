{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

module JSONRPC.Types.Method where

import Data.Aeson qualified as A
import Data.GADT.Compare
import Data.Kind
import Data.Singletons
import Data.Type.Equality

type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  All c '[] = ()
  All c (x ': xs) = (c x, All c xs)

-- TODO: write some TH to generate this
class Closed k where
  type Elements k :: [k]

  bring :: k `Everywhere` constr => proxy k -> Sing m -> (constr m => r) -> r

type Everywhere k (constr :: k -> Constraint) = All constr (Elements k)

data Role = Server | Client
  deriving (Show, Eq, Ord)

data SRole (r :: Role) where
  SServer :: SRole Server
  SClient :: SRole Client

type instance Sing = SRole

type family OtherRole (r :: Role) where
  OtherRole Server = Client
  OtherRole Client = Server

sOtherRole :: SRole r -> SRole (OtherRole r)
sOtherRole SServer = SClient
sOtherRole SClient = SServer

otherRoleInverse :: forall r . SingI r => (r :~: OtherRole (OtherRole r))
otherRoleInverse = case sing @r of
  SServer -> Refl
  SClient -> Refl

instance SingKind Role where
  type instance Demote Role = Role
  fromSing = \case
    SServer -> Server
    SClient -> Client
  toSing = \case
    Server -> SomeSing SServer
    Client -> SomeSing SClient

data MsgKind = Request | Notification

data SMsgKind k where
  SRequest :: SMsgKind Request
  SNotification :: SMsgKind Notification

type instance Sing = SMsgKind

data Initiator = ServerInitiates | ClientInitiates | EitherInitiates

data SInitiator (i :: Initiator) where
  SServerInitiates :: SInitiator ServerInitiates
  SClientInitiates :: SInitiator ClientInitiates
  SEitherInitiates :: SInitiator EitherInitiates
type instance Sing = SInitiator

class
  (A.ToJSON (MessageParams m)
  , A.FromJSON (MessageParams m)
  , A.ToJSON (MessageResult m)
  , A.FromJSON (MessageResult m)
  , A.ToJSON (ErrorData m)
  , A.FromJSON (ErrorData m)) => IsJsonParts m where
instance
  (A.ToJSON (MessageParams m)
  , A.FromJSON (MessageParams m)
  , A.ToJSON (MessageResult m)
  , A.FromJSON (MessageResult m)
  , A.ToJSON (ErrorData m)
  , A.FromJSON (ErrorData m)) => IsJsonParts m

class
  (Eq (MessageParams m)
  , Eq (MessageResult m)
  , Eq (ErrorData m)) => EqParts m where
instance
  (Eq (MessageParams m)
  , Eq (MessageResult m)
  , Eq (ErrorData m)) => EqParts m

class
  (Show (MessageParams m)
  , Show (MessageResult m)
  , Show (ErrorData m)) => ShowParts m where
instance
  (Show (MessageParams m)
  , Show (MessageResult m)
  , Show (ErrorData m)) => ShowParts m

class
  ( SingKind k
  , Demote k ~ k
  , A.FromJSON k
  , A.ToJSON k
  , Show k
  , Eq k
  , GCompare @k Sing
  , Closed k
  , k `Everywhere` IsJsonParts
  , k `Everywhere` EqParts
  , k `Everywhere` ShowParts
  ) =>
  Method k
  where
  type MessageInitiator (m :: k) :: Initiator
  sMessageInitiator :: forall (m :: k) . Sing m -> Sing (MessageInitiator m)

  type MessageKind (m :: k) :: MsgKind
  sMsgKind :: forall (m :: k). Sing m -> Sing (MessageKind m)

  type MessageParams (m :: k) :: Type
  type MessageResult (m :: k) :: Type
  type ErrorData (m :: k) :: Type

-- Just to avoid depending on singletons-base to get SBool
data Allowed = Yes | No

data SAllowed (a :: Allowed) where
  SYes :: SAllowed Yes
  SNo :: SAllowed No

type instance Sing = SAllowed

type family CanInitiate (r :: Role) (i :: Initiator) :: Allowed where
  CanInitiate Server ServerInitiates = Yes
  CanInitiate Server ClientInitiates = No
  CanInitiate Server EitherInitiates = Yes

  CanInitiate Client ServerInitiates = No
  CanInitiate Client ClientInitiates = Yes
  CanInitiate Client EitherInitiates = Yes

sCanInitiate :: Sing r -> Sing i -> Sing (CanInitiate r i)
sCanInitiate SServer SServerInitiates = SYes
sCanInitiate SServer SClientInitiates = SNo
sCanInitiate SServer SEitherInitiates = SYes
sCanInitiate SClient SServerInitiates = SNo
sCanInitiate SClient SClientInitiates = SYes
sCanInitiate SClient SEitherInitiates = SYes

type NotificationOk r m = (CanInitiate r (MessageInitiator m) ~ Yes, MessageKind m ~ Notification)
type RequestOk r m = (CanInitiate r (MessageInitiator m) ~ Yes, MessageKind m ~ Request)
type ResponseOk r m = (CanInitiate (OtherRole r) (MessageInitiator m) ~ Yes, MessageKind m ~ Request)
