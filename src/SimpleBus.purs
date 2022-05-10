module SimpleBus
  ( Bus
  , bus
  , raise
  , subscribe
  , testHelpers
  , unsubscribe
  ) where

import Prelude
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Atom (Atom, atom)
import Erl.Process (class HasSelf)

newtype Bus :: forall k. Type -> k -> Type
newtype Bus name msg
  = Bus name

bus :: forall msg name. name -> Bus name msg
bus = Bus

foreign import subscribeImpl :: forall name msgIn msgOut. Atom -> Bus name msgIn -> (msgIn -> msgOut) -> Effect Unit

type SubscribeAPI
  = forall m name msg msgOut. HasSelf m msgOut => MonadEffect m => Bus name msg -> (msg -> msgOut) -> m Unit

subscribe :: SubscribeAPI
subscribe onBus f = do
  liftEffect $ subscribeImpl (atom "enabled") onBus f

subscribeDisabled :: SubscribeAPI
subscribeDisabled onBus f = do
  liftEffect $ subscribeImpl (atom "disabled") onBus f

testHelpers ::
  forall name msg.
  { disable :: Bus name msg -> Effect Unit
  , enable :: Bus name msg -> Effect Unit
  , subscribeDisabled :: SubscribeAPI
  }
testHelpers =
  { enable: enable
  , disable: disable
  , subscribeDisabled: subscribeDisabled
  }

foreign import raise :: forall name msg. Bus name msg -> msg -> Effect Unit
foreign import enable :: forall name msg. Bus name msg -> Effect Unit
foreign import disable :: forall name msg. Bus name msg -> Effect Unit
foreign import unsubscribe :: forall name msg. Bus name msg -> Effect Unit
