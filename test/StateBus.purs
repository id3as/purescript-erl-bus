module Test.StateBus (stbTests) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Debug (spy)
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom (Atom, atom)
import Erl.Process (Process, ProcessM, receive, receiveWithTimeout, self, spawnLink, unsafeRunProcessM)
import Erl.Process as Process
import Erl.Test.EUnit as Test
import Partial.Unsafe (unsafeCrashWith)
import StateBus (class UpdateState, Bus, BusMsg(..), BusRef, Generation, InitialState, VersionedMsg, busRef, create, delete, raise, subscribe, subscribeExisting, unsubscribe, validateMsg)
import Test.Assert (assertEqual, assertEqual', assertTrue')
import Unsafe.Coerce (unsafeCoerce)

data StBMsg
  = TestMsg Int
derive instance Eq StBMsg
instance Show StBMsg where
  show (TestMsg i) = "testMsg " <> show i

data State
  = TestState Int
derive instance Eq State
instance Show State where
  show (TestState i) = "testState " <> show i

instance UpdateState State StBMsg where
  updateState (TestMsg m) (TestState s) = TestState $ s + m

data SenderMsg
  = RaiseMsg StBMsg
  | DeleteBus
  | End

data RunnerMsg
  = StateSet
  | MsgSent
  | Complete
  | SubscriberStepCompleted Int

derive instance Eq RunnerMsg
instance Show RunnerMsg where
  show StateSet = "StateSet"
  show MsgSent = "MsgSent"
  show Complete = "Complete"
  show (SubscriberStepCompleted i) = "SubscriberStepCompleted " <> show i

type SenderRequest
  = { req :: SenderMsg
    , resp :: Maybe RunnerMsg
    }

type SubscriberMsg
  = BusMsg StBMsg State

stbTests :: Test.TestSuite
stbTests = do
  Test.suite "state bus tests" do
    subscribeTests
    --subscribeExistingTests

subscribeTests :: Test.TestSuite
subscribeTests = do
  Test.suite "subscribe tests" do
    nonExistentBus
    -- createThenSubscribe
    -- canUpdateStatePriorToSubscription
    -- canUpdateStatePostSubscription
    -- canReceiveMessages
    -- afterUnsubscribeYouReceiveNoMessages
    -- terminateMessageWhenSenderDeletesBus
    -- terminateMessageWhenSenderExits

nonExistentBus :: Test.TestSuite
nonExistentBus = do
  Test.test "You can subscribe to a bus before it exists" do
    unsafeRunProcessM theTest
  where
  theTest :: ProcessM RunnerMsg Unit
  theTest = do
    me <- self
    _ <- liftEffect $ spawnLink $ subscriber me
    await $ SubscriberStepCompleted 0
    senderPid <- liftEffect $ spawnLink $ sender me (TestState 0) (Just StateSet)
    await StateSet
    liftEffect do
      Process.send senderPid { req: RaiseMsg (TestMsg 1), resp: Nothing }
      Process.send senderPid { req: RaiseMsg (TestMsg 2), resp: Nothing }
    await Complete
    liftEffect $ Process.send senderPid $ { req: End, resp: Nothing }
    pure unit

  subscriber :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    subscribe testBus Just
    liftEffect do
      Process.send parent (SubscriberStepCompleted 0)

    generation <- awaitInitialState (TestState 0)
    awaitValidatedMsg generation (TestMsg 1)
    awaitValidatedMsg generation (TestMsg 2)
    liftEffect $ Process.send parent Complete

unsafeGeneration :: Int -> Generation
unsafeGeneration = unsafeCoerce

{-
createThenSubscribe :: Test.TestSuite
createThenSubscribe = do
  Test.test "Can subscribe after a bus is created" do
    unsafeRunProcessM theTest
  where
  theTest :: ProcessM RunnerMsg Unit
  theTest = do
    me <- self
    senderPid <- liftEffect $ spawnLink $ sender me (TestState 0) (Just StateSet)
    await StateSet
    _ <- liftEffect $ spawnLink $ subscriber me
    await $ SubscriberStepCompleted 1
    liftEffect $ Process.send senderPid $ { req: End, resp: Nothing }
    pure unit

  subscriber :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    subscribe testBus Just
    await $ StateMsg (TestState 0)
    liftEffect $ Process.send parent (SubscriberStepCompleted 1)


canUpdateStatePriorToSubscription :: Test.TestSuite
canUpdateStatePriorToSubscription = do
  Test.test "On subscription, you are sent the most up to date state" do
    unsafeRunProcessM theTest
  where
  theTest :: ProcessM RunnerMsg Unit
  theTest = do
    me <- self
    senderPid <- liftEffect $ spawnLink $ sender me (TestState 0) Nothing
    liftEffect
      $ Process.send senderPid
          { req: SetState (TestState 1)
          , resp: Just StateSet
          }
    await StateSet
    _ <- liftEffect $ spawnLink $ subscriber me
    await Complete
    liftEffect $ Process.send senderPid $ { req: End, resp: Nothing } -- allow the sender to exit so we are clean for the next test
    pure unit

  subscriber :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    subscribe testBus pure
    await $ StateMsg (TestState 1)
    liftEffect $ Process.send parent Complete

canUpdateStatePostSubscription :: Test.TestSuite
canUpdateStatePostSubscription = do
  Test.test "Changes to state are sent to active subscribers" do
    unsafeRunProcessM theTest
  where
  theTest :: ProcessM RunnerMsg Unit
  theTest = do
    me <- self
    senderPid <- liftEffect $ spawnLink $ sender me (TestState 0) (Just StateSet)
    await StateSet
    _ <- liftEffect $ spawnLink $ subscriber me
    await $ SubscriberStepCompleted 0
    liftEffect
      $ Process.send senderPid { req: SetState (TestState 1), resp: Nothing }
    await $ SubscriberStepCompleted 1
    liftEffect $ Process.send senderPid $ { req: End, resp: Nothing }
    pure unit

  subscriber :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    subscribe testBus pure
    await $ StateMsg $TestState 0
    liftEffect $ Process.send parent (SubscriberStepCompleted 0)
    await $ StateMsg $TestState 1
    liftEffect $ Process.send parent (SubscriberStepCompleted 1)


canReceiveMessages :: Test.TestSuite
canReceiveMessages = do
  Test.test "Data messages are sent to active subscribers" do
    unsafeRunProcessM theTest
  where
  theTest :: ProcessM RunnerMsg Unit
  theTest = do
    me <- self
    senderPid <- liftEffect $ spawnLink $ sender me (TestState 0) (Just StateSet)
    await StateSet
    _ <- liftEffect $ spawnLink $ subscriber me
    await $ SubscriberStepCompleted 0
    liftEffect do
      Process.send senderPid { req: RaiseMsg (TestMsg 1), resp: Nothing }
      Process.send senderPid { req: RaiseMsg (TestMsg 2), resp: Nothing }
    await $ Complete
    liftEffect $ Process.send senderPid $ { req: End, resp: Nothing }
    pure unit

  subscriber :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    subscribe testBus pure
    await $ StateMsg $TestState 0
    liftEffect $ Process.send parent (SubscriberStepCompleted 0)
    await $ DataMsg $ TestMsg 1
    await $ DataMsg $ TestMsg 2
    liftEffect $ Process.send parent Complete

afterUnsubscribeYouReceiveNoMessages :: Test.TestSuite
afterUnsubscribeYouReceiveNoMessages = do
  Test.test "Data messages are no longer sent after unsubscribe" do
    unsafeRunProcessM theTest
  where
  theTest :: ProcessM RunnerMsg Unit
  theTest = do
    me <- self
    senderPid <- liftEffect $ spawnLink $ sender me (TestState 0) (Just StateSet)
    await StateSet
    _ <- liftEffect $ spawnLink $ subscriber me
    await $ SubscriberStepCompleted 0
    liftEffect $ Process.send senderPid { req: RaiseMsg (TestMsg 1), resp: Nothing }
    await $ SubscriberStepCompleted 1
    liftEffect $ Process.send senderPid { req: RaiseMsg (TestMsg 2), resp: Nothing }
    await $ Complete
    liftEffect $ Process.send senderPid $ { req: End, resp: Nothing }

  subscriber :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    subscribe testBus pure
    await $ StateMsg $TestState 0
    liftEffect $ Process.send parent (SubscriberStepCompleted 0)
    await $ DataMsg $ TestMsg 1
    liftEffect do
      unsubscribe testBus
      Process.send parent (SubscriberStepCompleted 1)
    awaitWithTimeout (Milliseconds 10.0) (DataMsg $ TestMsg 999) (DataMsg $ TestMsg 999)
    liftEffect $ Process.send parent Complete

terminateMessageWhenSenderDeletesBus  :: Test.TestSuite
terminateMessageWhenSenderDeletesBus = do
  Test.test "Subscribers are notified when the sender exists" do
    unsafeRunProcessM theTest
  where
  theTest :: ProcessM RunnerMsg Unit
  theTest = do
    me <- self
    senderPid <- liftEffect $ spawnLink $ sender me (TestState 0) (Just StateSet)
    await StateSet
    _ <- liftEffect $ spawnLink $ subscriber me
    await $ SubscriberStepCompleted 0
    liftEffect $ Process.send senderPid { req: RaiseMsg (TestMsg 1), resp: Nothing }
    liftEffect $ Process.send senderPid { req: DeleteBus, resp: Nothing }
    await $ Complete
    liftEffect $ Process.send senderPid $ { req: End, resp: Nothing }

  subscriber :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    subscribe testBus pure
    await $ StateMsg $TestState 0
    liftEffect $ Process.send parent (SubscriberStepCompleted 0)
    await $ DataMsg $ TestMsg 1
    await $ BusTerminated
    liftEffect $ Process.send parent Complete


terminateMessageWhenSenderExits  :: Test.TestSuite
terminateMessageWhenSenderExits = do
  Test.test "Subscribers are notified when the sender exists" do
    unsafeRunProcessM theTest
  where
  theTest :: ProcessM RunnerMsg Unit
  theTest = do
    me <- self
    senderPid <- liftEffect $ spawnLink $ sender me (TestState 0) (Just StateSet)
    await StateSet
    _ <- liftEffect $ spawnLink $ subscriber me
    await $ SubscriberStepCompleted 0
    liftEffect $ Process.send senderPid { req: RaiseMsg (TestMsg 1), resp: Nothing }
    liftEffect $ Process.send senderPid $ { req: End, resp: Nothing }
    await $ Complete

  subscriber :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    subscribe testBus pure
    await $ StateMsg $TestState 0
    liftEffect $ Process.send parent (SubscriberStepCompleted 0)
    await $ DataMsg $ TestMsg 1
    await $ BusTerminated
    liftEffect $ Process.send parent Complete


subscribeExistingTests :: Test.TestSuite
subscribeExistingTests = do
  Test.suite "subscribeExisting tests" do
    seNonExistentBus
    seCreateThenSubscribe
    seCanUpdateStatePriorToSubscription
    seCanUpdateStatePostSubscription
    seCanReceiveMessages

seNonExistentBus :: Test.TestSuite
seNonExistentBus = do
  Test.test "Subscribing to a non-existent bus returns nothing" do
    unsafeRunProcessM theTest
  where
  theTest :: ProcessM RunnerMsg Unit
  theTest = do
    me <- self
    _ <- liftEffect $ spawnLink $ subscriber me
    await Complete
    pure unit

  subscriber :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    res <- subscribeExisting testBus pure
    liftEffect do
      assertNothing' "Should have no initial state" res
      Process.send parent Complete
    pure unit

seCreateThenSubscribe :: Test.TestSuite
seCreateThenSubscribe = do
  Test.test "Can subscribeExisting once a bus is created" do
    unsafeRunProcessM theTest
  where
  theTest :: ProcessM RunnerMsg Unit
  theTest = do
    me <- self
    senderPid <- liftEffect $ spawnLink $ sender me (TestState 0) (Just StateSet)
    await StateSet
    _ <- liftEffect $ spawnLink $ subscriber me
    await Complete
    liftEffect $ Process.send senderPid $ { req: End, resp: Nothing }
    pure unit

  subscriber :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    res <- subscribeExisting testBus pure
    liftEffect do
      assertEqual' "Initial state matches" { actual: res, expected: Just $ TestState 0 }
      Process.send parent Complete
    pure unit

seCanUpdateStatePriorToSubscription :: Test.TestSuite
seCanUpdateStatePriorToSubscription = do
  Test.test "On subscription, you get the most up to date state" do
    unsafeRunProcessM theTest
  where
  theTest :: ProcessM RunnerMsg Unit
  theTest = do
    me <- self
    senderPid <- liftEffect $ spawnLink $ sender me (TestState 0) Nothing
    liftEffect
      $ Process.send senderPid
          { req: SetState (TestState 1)
          , resp: Just StateSet
          }
    await StateSet
    _ <- liftEffect $ spawnLink $ subscriber me
    await Complete
    liftEffect $ Process.send senderPid $ { req: End, resp: Nothing } -- allow the sender to exit so we are clean for the next test
    pure unit

  subscriber :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    res <- subscribeExisting testBus pure
    liftEffect do
      assertEqual' "Initial state has been updated" { actual: res, expected: Just $ TestState 1 }
      Process.send parent Complete
    pure unit

seCanUpdateStatePostSubscription :: Test.TestSuite
seCanUpdateStatePostSubscription = do
  Test.test "Changes to state are sent to active subscribers" do
    unsafeRunProcessM theTest
  where
  theTest :: ProcessM RunnerMsg Unit
  theTest = do
    me <- self
    senderPid <- liftEffect $ spawnLink $ sender me (TestState 0) (Just StateSet)
    await StateSet
    _ <- liftEffect $ spawnLink $ subscriber me
    await $ SubscriberStepCompleted 0
    liftEffect
      $ Process.send senderPid { req: SetState (TestState 1), resp: Nothing }
    await $ SubscriberStepCompleted 1
    liftEffect $ Process.send senderPid $ { req: End, resp: Nothing }
    pure unit

  subscriber :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    res <- subscribeExisting testBus pure
    liftEffect do
      assertEqual' "Initial state received" { actual: res, expected: Just $ TestState 0 }
      Process.send parent (SubscriberStepCompleted 0)
    await $ StateMsg $TestState 1
    liftEffect $ Process.send parent (SubscriberStepCompleted 1)

seCanReceiveMessages :: Test.TestSuite
seCanReceiveMessages = do
  Test.test "Data messages are sent to active subscribers" do
    unsafeRunProcessM theTest
  where
  theTest :: ProcessM RunnerMsg Unit
  theTest = do
    me <- self
    senderPid <- liftEffect $ spawnLink $ sender me (TestState 0) (Just StateSet)
    await StateSet
    _ <- liftEffect $ spawnLink $ subscriber me
    await $ SubscriberStepCompleted 0
    liftEffect do
      Process.send senderPid { req: RaiseMsg (TestMsg 1), resp: Nothing }
      Process.send senderPid { req: RaiseMsg (TestMsg 2), resp: Nothing }
    await $ Complete
    liftEffect $ Process.send senderPid $ { req: End, resp: Nothing }
    pure unit

  subscriber :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    res <- subscribeExisting testBus pure
    liftEffect do
      assertEqual' "Initial state received" { actual: res, expected: Just $ TestState 0 }
      Process.send parent (SubscriberStepCompleted 0)
    await $ DataMsg $ TestMsg 1
    await $ DataMsg $ TestMsg 2
    liftEffect $ Process.send parent Complete

-}

awaitInitialState ∷ ∀ (msg ∷ Type) (state :: Type). Eq state ⇒ Show state ⇒ state → ProcessM (BusMsg msg  state) Generation
awaitInitialState expected = do
  msg <- receive
  case spy "msg" msg of
    InitialStateMsg {generation, state} -> do
      liftEffect $ assertEqual { actual: state, expected }
      pure generation
    _ ->
      unsafeCrashWith "Not initial state" { actual: msg, expected }



await ∷ ∀ (a ∷ Type). Eq a ⇒ Show a ⇒ a → ProcessM a Unit
await what = do
  msg <- receive
  liftEffect $ assertEqual { actual: msg, expected: what }

awaitValidatedMsg ∷ ∀ (msg ∷ Type) (state :: Type). Eq msg ⇒ Show msg ⇒ Generation -> msg → ProcessM (BusMsg msg  state) Unit
awaitValidatedMsg generation what = do
  msg <- receive
  case spy "awaitValidatedMsg" msg of
    DataMsg versionedMessage -> do
      let validated = validateMsg generation versionedMessage
      liftEffect $ assertEqual { actual: validated, expected: Just what }
    _ ->
      unsafeCrashWith "Not versioned msg" { actual: msg, expected: what }


awaitWithTimeout ∷ ∀ (a ∷ Type). Eq a ⇒ Show a ⇒ Milliseconds -> a -> a → ProcessM a Unit
awaitWithTimeout duration toMsg expected = do
  msg <- receiveWithTimeout duration toMsg
  liftEffect $ assertEqual { actual: msg, expected }

sender :: Process RunnerMsg -> State -> Maybe RunnerMsg -> ProcessM SenderRequest Unit
sender parent initialMd initResp = do
  theBus <-
    liftEffect do
      bus <- create testBus initialMd
      maybeRespond initResp
      pure bus
  senderLoop theBus

  where
  senderLoop :: Bus Atom StBMsg State -> ProcessM SenderRequest Unit
  senderLoop bus = do
    msg <- receive
    case msg.req of
      RaiseMsg a -> do
        liftEffect do
          raise bus a
          maybeRespond msg.resp
        senderLoop bus
      DeleteBus -> do
        liftEffect do
          delete bus
          maybeRespond msg.resp
        senderLoop bus
      End ->
        liftEffect do
          maybeRespond msg.resp
          pure unit


  maybeRespond Nothing = pure unit
  maybeRespond (Just msg) = Process.send parent msg

testBus :: BusRef Atom StBMsg State
testBus = busRef (atom "test-bus")

assertNothing' ∷ ∀ a. String → Maybe a -> Effect Unit
assertNothing' msg =
  case _ of
    Nothing ->
      pure unit
    Just _ -> do
      assertTrue' msg false
