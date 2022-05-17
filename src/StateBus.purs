module StateBus
  ( class UpdateState
  , updateState
  , Bus
  , BusMsg(..)
  , BusRef
  , Generation
  , InitialState
  , VersionedMsg
  , busRef
  , create
  , delete
  , raise
  , subscribe
  , subscribeExisting
  , unsubscribe
  , validateMsg
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Process (class HasSelf)

class UpdateState state msg where
  updateState :: msg -> state -> state


newtype Bus :: Type -> Type -> Type -> Type
newtype Bus name msg state
  = Bus name

newtype BusRef :: Type -> Type -> Type -> Type
newtype BusRef name msg state
  = BusRef name
instance eqBusRef :: (Eq name) => Eq (BusRef name msg state) where
  eq (BusRef name1) (BusRef name2) = eq name1 name2

busRef :: forall name msg state. name -> BusRef name msg state
busRef = BusRef

newtype Generation = Generation Int

data VersionedMsg :: forall k. Type -> k -> Type
data VersionedMsg msg state = VersionedMsg Generation msg

type InitialState state = {state :: state, generation :: Generation}

data BusMsg msg state
  = DataMsg (VersionedMsg msg state)
  | InitialStateMsg (InitialState state)
  | BusTerminated

derive newtype instance Eq Generation
derive newtype instance Ord Generation
derive instance (Eq msg) => Eq (VersionedMsg msg state)
derive instance (Eq msg, Eq state) => Eq (BusMsg msg state)

validateMsg :: forall msg state. Generation -> VersionedMsg msg state -> Maybe msg
validateMsg initialStateGeneration (VersionedMsg generation msg) =
  if generation > initialStateGeneration then
    Just msg
  else
    Nothing

instance Show (Generation) where
  show (Generation gen) = "Generation " <> show gen

instance (Show msg) => Show (VersionedMsg msg state) where
  show (VersionedMsg generation msg ) = "VersionedMsg " <> show generation <> " / " <> show msg

instance (Show msg, Show state) => Show (BusMsg msg state) where
  show (DataMsg  versionedMsg) = "DataMsg " <> show versionedMsg
  show (InitialStateMsg state) = "InitialStateMsg " <> show state
  show BusTerminated = "BusTerminated"


foreign import subscribeImpl :: forall name msg state msgOut. (InitialState state -> BusMsg msg state) -> BusRef name msg state -> (BusMsg msg state -> Maybe msgOut) -> Effect Unit
foreign import subscribeExistingImpl :: forall name msg state msgOut. BusRef name msg state -> (BusMsg msg state -> Maybe msgOut) -> Effect (Maybe (InitialState state))

type SubscribeAPI
  = forall m name msg state msgOut. HasSelf m msgOut => MonadEffect m => BusRef name msg state -> (BusMsg msg state -> Maybe msgOut) -> m Unit

type SubscribeExistingAPI
  = forall m name msg state msgOut. HasSelf m msgOut => MonadEffect m => BusRef name msg state -> (BusMsg msg state -> Maybe msgOut) -> m (Maybe (InitialState state))

foreign import create :: forall name msg state. BusRef name msg state -> state -> Effect (Bus name msg state)
foreign import delete :: forall name msg state. Bus name msg state -> Effect Unit

subscribe :: SubscribeAPI
subscribe onBus f = do
  liftEffect $ subscribeImpl (InitialStateMsg) onBus f

subscribeExisting :: SubscribeExistingAPI
subscribeExisting onBus f = do
  liftEffect $ subscribeExistingImpl onBus f

raise :: forall name msg state. UpdateState state msg => Bus name msg state -> msg -> Effect Unit
raise = raiseImpl updateState

foreign import raiseImpl :: forall name msg state. (msg -> state -> state) -> Bus name msg state -> msg -> Effect Unit
foreign import unsubscribe :: forall name msg state. BusRef name msg state -> Effect Unit
