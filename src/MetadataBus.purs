module MetadataBus
  ( Bus
  , BusMsg(..)
  , BusRef
  , busRef
  , create
  , delete
  , raise
  , subscribe
  , subscribeExisting
  , updateMetadata
  --, testHelpers
  , unsubscribe
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Atom (Atom, atom)
import Erl.Process (class HasSelf)

newtype Bus :: Type -> Type -> Type -> Type
newtype Bus name msg metadata
  = Bus name

newtype BusRef :: Type -> Type -> Type -> Type
newtype BusRef name msg metadata
  = BusRef name
instance eqBusRef :: (Eq name) => Eq (BusRef name msg metadata) where
  eq (BusRef name1) (BusRef name2) = eq name1 name2

busRef :: forall name msg metadata. name -> BusRef name msg metadata
busRef = BusRef

data BusMsg msg metadata
  = DataMsg msg
  | MetadataMsg metadata
  | BusTerminated

derive instance (Eq msg, Eq metadata) => Eq (BusMsg msg metadata)


instance (Show msg, Show metadata) => Show (BusMsg msg metadata) where
  show (DataMsg msg) = "DataMsg " <> show msg
  show (MetadataMsg md) = "MetadataMsg " <> show md
  show BusTerminated = "BusTerminated"


foreign import subscribeImpl :: forall name msg metadata msgOut. Atom -> (metadata -> BusMsg msg metadata) -> BusRef name msg metadata -> (BusMsg msg metadata -> Maybe msgOut) -> Effect Unit
foreign import subscribeExistingImpl :: forall name msg metadata msgOut. Atom -> BusRef name msg metadata -> (BusMsg msg metadata -> Maybe msgOut) -> Effect (Maybe metadata)

type SubscribeAPI
  = forall m name msg metadata msgOut. HasSelf m msgOut => MonadEffect m => BusRef name msg metadata -> (BusMsg msg metadata -> Maybe msgOut) -> m Unit


type SubscribeExistingAPI
  = forall m name msg metadata msgOut. HasSelf m msgOut => MonadEffect m => BusRef name msg metadata -> (BusMsg msg metadata -> Maybe msgOut) -> m (Maybe metadata)

foreign import create :: forall name msg metadata. BusRef name msg metadata -> metadata -> Effect (Bus name msg metadata)
foreign import delete :: forall name msg metadata. Bus name msg metadata -> Effect Unit
foreign import updateMetadata :: forall name msg metadata. Bus name msg metadata -> metadata -> Effect Unit

subscribe :: SubscribeAPI
subscribe onBus f = do
  liftEffect $ subscribeImpl (atom "enabled") MetadataMsg onBus f

subscribeExisting :: SubscribeExistingAPI
subscribeExisting onBus f = do
  liftEffect $ subscribeExistingImpl (atom "enabled") onBus f

subscribeDisabled :: SubscribeExistingAPI
subscribeDisabled onBus f = do
  liftEffect $ subscribeExistingImpl (atom "disabled") onBus f

raise :: forall name msg metadata. Bus name msg metadata -> msg -> Effect Unit
raise bus = raiseMsg bus <<< DataMsg
-- testHelpers ::
--   forall name msg.
--   { disable :: Bus name msg -> Effect Unit
--   , enable :: Bus name msg -> Effect Unit
--   , subscribeDisabled :: SubscribeExistingAPI
--   }
-- testHelpers =
--   { enable: enable
--   , disable: disable
--   , subscribeDisabled: subscribeDisabled
--   }
foreign import raiseMsg :: forall name msg metadata. Bus name msg metadata -> BusMsg msg metadata -> Effect Unit
foreign import enable :: forall name msg metadata. Bus name msg metadata -> Effect Unit
foreign import disable :: forall name msg metadata. Bus name msg metadata -> Effect Unit
foreign import unsubscribe :: forall name msg metadata. BusRef name msg metadata -> Effect Unit
