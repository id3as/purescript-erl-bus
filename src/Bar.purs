module Bar
where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign (Foreign)
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)


newtype ProcessM (msg :: Type) res
  = ProcessM (Effect res)
derive newtype instance functorProcessM :: Functor (ProcessM res)
derive newtype instance applyProcessM :: Apply (ProcessM res)
derive newtype instance applicativeProcessM :: Applicative (ProcessM res)
derive newtype instance bindProcessM :: Bind (ProcessM res)
derive newtype instance monadProcessM :: Monad (ProcessM res)

instance MonadEffect (ProcessM a) where
  liftEffect = ProcessM


rawReceive :: Effect Foreign
rawReceive = unsafeCoerce 1

data TrapExitMsg = TrapExitMsg
newtype TrapExitT m msg res = TrapExitT (m msg res)

instance MonadEffect (TrapExitT m msg) where
  liftEffect = unsafeCoerce


instance Functor (TrapExitT m msg) where
  map = unsafeCoerce
instance Apply (TrapExitT m msg) where
  apply = unsafeCoerce
instance Applicative (TrapExitT m msg) where
  pure = unsafeCoerce
instance Bind (TrapExitT m msg) where
  bind = unsafeCoerce
instance Monad (TrapExitT m msg)

instance
  FFIParseT (m msg) msg2 =>
  FFIParseT (TrapExitT m msg) (Either TrapExitMsg msg2) where
  psFromFFI :: _ -> Foreign -> Either TrapExitMsg msg2
  psFromFFI _ fgn = do
    case parseExitMsg fgn of
      Just psMsg -> Left psMsg
      Nothing -> Right $ psFromFFI (Proxy ::_ (m msg)) fgn

instance
  ( FFIParseT (m msg) msg2
  )
  => ReceiveT (TrapExitT m msg) (Either TrapExitMsg msg2) where
  receive = do
    psFromFFI (Proxy :: _ (TrapExitT m msg)) <$> liftEffect rawReceive



data MonitorMsg = MonitorMsg

newtype MonitorT m msg res = MonitorT (m msg res)

instance MonadEffect (MonitorT m msg) where
  liftEffect = unsafeCoerce

instance Functor (MonitorT m msg) where
  map = unsafeCoerce
instance Apply (MonitorT m msg) where
  apply = unsafeCoerce
instance Applicative (MonitorT m msg) where
  pure = unsafeCoerce
instance Bind (MonitorT m msg) where
  bind = unsafeCoerce
instance Monad (MonitorT m msg)

instance
  FFIParseT (m msg) msg2 =>
  FFIParseT (MonitorT m msg) (Either MonitorMsg msg2) where
  psFromFFI :: _ -> Foreign -> Either MonitorMsg msg2
  psFromFFI _ fgn = do
    case parseMonitorMsg fgn of
      Just monitorMsg -> Left monitorMsg
      Nothing -> Right $ psFromFFI (Proxy ::_ (m msg)) fgn



foreign import parseMonitorMsg :: Foreign -> Maybe MonitorMsg
foreign import parseExitMsg :: Foreign -> Maybe TrapExitMsg

instance
  ( FFIParseT (m msg) msg2
  )
  => ReceiveT (MonitorT m msg) (Either MonitorMsg msg2) where
  receive = do
    psFromFFI (Proxy :: _ (MonitorT m msg)) <$> liftEffect rawReceive

-- y ------------------------------------------------------------------------
-- m is TrapExitT ProcessM
-- msg is AppMsg

data AppMsg = AppMsg

class FFIParseT m outMsg | m -> outMsg where
  psFromFFI :: Proxy m  -> Foreign -> outMsg

class ReceiveT m msg | m -> msg where
  receive :: m msg
-- y----
-- m is MonitorT (TrapExitT ProcessM) AppMsg
-- msg  Either MonitorMsg (Either TrapExitMsg AppMsg)

y :: MonitorT (TrapExitT ProcessM) AppMsg Unit
y = do
  msg :: Either MonitorMsg (Either TrapExitMsg AppMsg) <- receive
  case msg of
    Left MonitorMsg -> pure unit
    Right myApplevelMsg -> pure unit



xoo :: TrapExitT ProcessM AppMsg Unit
xoo = do
  msg :: Either TrapExitMsg AppMsg <- receive
  case msg of
    Left TrapExitMsg -> pure unit
    Right myApplevelMsg -> pure unit





-- t0 -> TrapExitT m1 msg2 (Either TrapExitMsg msg2)
--psFromFFILadder :: forall unhandled. FFIParseT mNext unhandled (Either handled unhandled) -> mNext mNext msg2 (Either TrapExitMsg msg2)
-- psFromFFILadder ::
--   forall ourPs appMsg m1 m2 outMsg.
--   FFIParseT (m1 m2 appMsg) (Either ourPs outMsg) =>
--   Monad (m1 m2 appMsg) =>
--   FFIParseT (m2 appMsg) outMsg =>
--   Foreign -> m1 m2 appMsg (Either ourPs outMsg)
-- psFromFFILadder inMsg =
--   case psFromFFI (Proxy :: _ (m1 m2 appMsg)) inMsg of
--     Left trapExitMsg ->
--       pure $  Left trapExitMsg
--     Right unhandled  ->
--       pure $ Right $ psFromFFI (Proxy :: _ (m2 appMsg)) unhandled






instance FFIParseT (ProcessM outMsg) outMsg where
  psFromFFI :: Proxy _ -> Foreign -> outMsg
  psFromFFI  = unsafeCoerce

    -- where
    --  psFromFFILadder inMsg =
    --   case psFromFFI (Proxy :: _ TrapExitT)  inMsg of
    --       Left trapExitMsg ->
    --         pure $  Left trapExitMsg
    --       Right (unhandled :: unhandled) ->
    --         pure $ Right $ psFromFFI (Proxy :: _ m)  unhandled

    


{-

instance :: Monad m => Functor (TrapExitT m) where
  map f =
instance Monad m => Applicative (TrapExitT m) where
  pure = TrapExitT <<< pure <<< Right



--newtype MonitorT m msg = MonitorT (m (Either MonitorMsg msg))






instance :: FFIParseT psMsg msg => FFIParseT (ExceptT m)

newtype ExceptT e m a = ExceptT (m (Either e a))



-- | The inverse of `ExceptT`. Run a computation in the `ExceptT` monad.
runTrapExitT :: forall m msg. TrapExitT m msg -> m (Either e a)
runExceptT (ExceptT x) = x

-}
{-
(\(MonitorT mapOfMappers inner) ->
X <- receive
   Case ffiMatch X of
    Just {ref, y} -> Left $ (Map.lookup ref mapOfMappers) y
    Nothing -> Right $ inner y

ffiMatch(x) ->
  Case x of
    {‘DOWN’, bla} -> just x
    _ -> Nothing


xoo :: GunT ( MonitorT (ProcessM msg ))
xoo =
  x :: ?w <- receive
  Case x of
    Left x -> process_gun x
    Right (Left x) -> process_monitor x
    Right (Right x) -> process_msg x
-}
