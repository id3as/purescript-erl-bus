module Test.MetadataBus (mbTests) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom (Atom, atom)
import Erl.Process (Process, ProcessM, receive, receiveWithTimeout, self, spawnLink, unsafeRunProcessM)
import Erl.Process as Process
import Erl.Test.EUnit as Test
import MetadataBus (Bus, BusMsg(..), BusRef, busRef, create, delete, raise, subscribe, subscribeExisting, unsubscribe, updateMetadata)
import Test.Assert (assertEqual, assertEqual', assertTrue')

data MbMsg
  = TestMsg Int
derive instance Eq MbMsg
instance Show MbMsg where
  show (TestMsg i) = "testMsg " <> show i

data Metadata
  = TestMetadata Int
derive instance Eq Metadata
instance Show Metadata where
  show (TestMetadata i) = "testMetadata " <> show i

data SenderMsg
  = SetMetadata Metadata
  | RaiseMsg MbMsg
  | DeleteBus
  | End

data RunnerMsg
  = MetadataSet
  | MsgSent
  | Complete
  | SubscriberStepCompleted Int

derive instance Eq RunnerMsg
instance Show RunnerMsg where
  show MetadataSet = "MetadataSet"
  show MsgSent = "MsgSent"
  show Complete = "Complete"
  show (SubscriberStepCompleted i) = "SubscriberStepCompleted " <> show i

type SenderRequest
  = { req :: SenderMsg
    , resp :: Maybe RunnerMsg
    }

type SubscriberMsg
  = BusMsg MbMsg Metadata

mbTests :: Test.TestSuite
mbTests = do
  Test.suite "metadata bus tests" do
    subscribeTests
    subscribeExistingTests

subscribeTests :: Test.TestSuite
subscribeTests = do
  Test.suite "subscribeExisting tests" do
    nonExistentBus
    createThenSubscribe
    canUpdateMetadataPriorToSubscription
    canUpdateMetadataPostSubscription
    canReceiveMessages
    afterUnsubscribeYouReceiveNoMessages
    terminateMessageWhenSenderDeletesBus
    terminateMessageWhenSenderExits

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
    senderPid <- liftEffect $ spawnLink $ sender me (TestMetadata 0) (Just MetadataSet)
    await MetadataSet
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
    await (MetadataMsg (TestMetadata 0))
    await (DataMsg (TestMsg 1))
    await (DataMsg (TestMsg 2))
    liftEffect $ Process.send parent Complete


createThenSubscribe :: Test.TestSuite
createThenSubscribe = do
  Test.test "Can subscribe after a bus is created" do
    unsafeRunProcessM theTest
  where
  theTest :: ProcessM RunnerMsg Unit
  theTest = do
    me <- self
    senderPid <- liftEffect $ spawnLink $ sender me (TestMetadata 0) (Just MetadataSet)
    await MetadataSet
    _ <- liftEffect $ spawnLink $ subscriber me
    await $ SubscriberStepCompleted 1
    liftEffect $ Process.send senderPid $ { req: End, resp: Nothing }
    pure unit

  subscriber :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    subscribe testBus Just
    await $ MetadataMsg (TestMetadata 0)
    liftEffect $ Process.send parent (SubscriberStepCompleted 1)


canUpdateMetadataPriorToSubscription :: Test.TestSuite
canUpdateMetadataPriorToSubscription = do
  Test.test "On subscription, you are sent the most up to date metadata" do
    unsafeRunProcessM theTest
  where
  theTest :: ProcessM RunnerMsg Unit
  theTest = do
    me <- self
    senderPid <- liftEffect $ spawnLink $ sender me (TestMetadata 0) Nothing
    liftEffect
      $ Process.send senderPid
          { req: SetMetadata (TestMetadata 1)
          , resp: Just MetadataSet
          }
    await MetadataSet
    _ <- liftEffect $ spawnLink $ subscriber me
    await Complete
    liftEffect $ Process.send senderPid $ { req: End, resp: Nothing } -- allow the sender to exit so we are clean for the next test
    pure unit

  subscriber :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    subscribe testBus pure
    await $ MetadataMsg (TestMetadata 1)
    liftEffect $ Process.send parent Complete

canUpdateMetadataPostSubscription :: Test.TestSuite
canUpdateMetadataPostSubscription = do
  Test.test "Changes to metadata are sent to active subscribers" do
    unsafeRunProcessM theTest
  where
  theTest :: ProcessM RunnerMsg Unit
  theTest = do
    me <- self
    senderPid <- liftEffect $ spawnLink $ sender me (TestMetadata 0) (Just MetadataSet)
    await MetadataSet
    _ <- liftEffect $ spawnLink $ subscriber me
    await $ SubscriberStepCompleted 0
    liftEffect
      $ Process.send senderPid { req: SetMetadata (TestMetadata 1), resp: Nothing }
    await $ SubscriberStepCompleted 1
    liftEffect $ Process.send senderPid $ { req: End, resp: Nothing }
    pure unit

  subscriber :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    subscribe testBus pure
    await $ MetadataMsg $TestMetadata 0
    liftEffect $ Process.send parent (SubscriberStepCompleted 0)
    await $ MetadataMsg $TestMetadata 1
    liftEffect $ Process.send parent (SubscriberStepCompleted 1)


canReceiveMessages :: Test.TestSuite
canReceiveMessages = do
  Test.test "Data messages are sent to active subscribers" do
    unsafeRunProcessM theTest
  where
  theTest :: ProcessM RunnerMsg Unit
  theTest = do
    me <- self
    senderPid <- liftEffect $ spawnLink $ sender me (TestMetadata 0) (Just MetadataSet)
    await MetadataSet
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
    await $ MetadataMsg $TestMetadata 0
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
    senderPid <- liftEffect $ spawnLink $ sender me (TestMetadata 0) (Just MetadataSet)
    await MetadataSet
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
    await $ MetadataMsg $TestMetadata 0
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
    senderPid <- liftEffect $ spawnLink $ sender me (TestMetadata 0) (Just MetadataSet)
    await MetadataSet
    _ <- liftEffect $ spawnLink $ subscriber me
    await $ SubscriberStepCompleted 0
    liftEffect $ Process.send senderPid { req: RaiseMsg (TestMsg 1), resp: Nothing }
    liftEffect $ Process.send senderPid { req: DeleteBus, resp: Nothing }
    await $ Complete
    liftEffect $ Process.send senderPid $ { req: End, resp: Nothing }

  subscriber :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    subscribe testBus pure
    await $ MetadataMsg $TestMetadata 0
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
    senderPid <- liftEffect $ spawnLink $ sender me (TestMetadata 0) (Just MetadataSet)
    await MetadataSet
    _ <- liftEffect $ spawnLink $ subscriber me
    await $ SubscriberStepCompleted 0
    liftEffect $ Process.send senderPid { req: RaiseMsg (TestMsg 1), resp: Nothing }
    liftEffect $ Process.send senderPid $ { req: End, resp: Nothing }
    await $ Complete

  subscriber :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    subscribe testBus pure
    await $ MetadataMsg $TestMetadata 0
    liftEffect $ Process.send parent (SubscriberStepCompleted 0)
    await $ DataMsg $ TestMsg 1
    await $ BusTerminated
    liftEffect $ Process.send parent Complete


subscribeExistingTests :: Test.TestSuite
subscribeExistingTests = do
  Test.suite "subscribeExisting tests" do
    seNonExistentBus
    seCreateThenSubscribe
    seCanUpdateMetadataPriorToSubscription
    seCanUpdateMetadataPostSubscription
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
      assertNothing' "Should have no initial metadata" res
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
    senderPid <- liftEffect $ spawnLink $ sender me (TestMetadata 0) (Just MetadataSet)
    await MetadataSet
    _ <- liftEffect $ spawnLink $ subscriber me
    await Complete
    liftEffect $ Process.send senderPid $ { req: End, resp: Nothing }
    pure unit

  subscriber :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    res <- subscribeExisting testBus pure
    liftEffect do
      assertEqual' "Initial metadata matches" { actual: res, expected: Just $ TestMetadata 0 }
      Process.send parent Complete
    pure unit

seCanUpdateMetadataPriorToSubscription :: Test.TestSuite
seCanUpdateMetadataPriorToSubscription = do
  Test.test "On subscription, you get the most up to date metadata" do
    unsafeRunProcessM theTest
  where
  theTest :: ProcessM RunnerMsg Unit
  theTest = do
    me <- self
    senderPid <- liftEffect $ spawnLink $ sender me (TestMetadata 0) Nothing
    liftEffect
      $ Process.send senderPid
          { req: SetMetadata (TestMetadata 1)
          , resp: Just MetadataSet
          }
    await MetadataSet
    _ <- liftEffect $ spawnLink $ subscriber me
    await Complete
    liftEffect $ Process.send senderPid $ { req: End, resp: Nothing } -- allow the sender to exit so we are clean for the next test
    pure unit

  subscriber :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    res <- subscribeExisting testBus pure
    liftEffect do
      assertEqual' "Initial metadata has been updated" { actual: res, expected: Just $ TestMetadata 1 }
      Process.send parent Complete
    pure unit

seCanUpdateMetadataPostSubscription :: Test.TestSuite
seCanUpdateMetadataPostSubscription = do
  Test.test "Changes to metadata are sent to active subscribers" do
    unsafeRunProcessM theTest
  where
  theTest :: ProcessM RunnerMsg Unit
  theTest = do
    me <- self
    senderPid <- liftEffect $ spawnLink $ sender me (TestMetadata 0) (Just MetadataSet)
    await MetadataSet
    _ <- liftEffect $ spawnLink $ subscriber me
    await $ SubscriberStepCompleted 0
    liftEffect
      $ Process.send senderPid { req: SetMetadata (TestMetadata 1), resp: Nothing }
    await $ SubscriberStepCompleted 1
    liftEffect $ Process.send senderPid $ { req: End, resp: Nothing }
    pure unit

  subscriber :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    res <- subscribeExisting testBus pure
    liftEffect do
      assertEqual' "Initial metadata received" { actual: res, expected: Just $ TestMetadata 0 }
      Process.send parent (SubscriberStepCompleted 0)
    await $ MetadataMsg $TestMetadata 1
    liftEffect $ Process.send parent (SubscriberStepCompleted 1)

seCanReceiveMessages :: Test.TestSuite
seCanReceiveMessages = do
  Test.test "Data messages are sent to active subscribers" do
    unsafeRunProcessM theTest
  where
  theTest :: ProcessM RunnerMsg Unit
  theTest = do
    me <- self
    senderPid <- liftEffect $ spawnLink $ sender me (TestMetadata 0) (Just MetadataSet)
    await MetadataSet
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
      assertEqual' "Initial metadata received" { actual: res, expected: Just $ TestMetadata 0 }
      Process.send parent (SubscriberStepCompleted 0)
    await $ DataMsg $ TestMsg 1
    await $ DataMsg $ TestMsg 2
    liftEffect $ Process.send parent Complete


await ∷ ∀ (a ∷ Type). Eq a ⇒ Show a ⇒ a → ProcessM a Unit
await what = do
  msg <- receive
  liftEffect $ assertEqual { actual: msg, expected: what }

awaitWithTimeout ∷ ∀ (a ∷ Type). Eq a ⇒ Show a ⇒ Milliseconds -> a -> a → ProcessM a Unit
awaitWithTimeout duration toMsg expected = do
  msg <- receiveWithTimeout duration toMsg
  liftEffect $ assertEqual { actual: msg, expected }

sender :: Process RunnerMsg -> Metadata -> Maybe RunnerMsg -> ProcessM SenderRequest Unit
sender parent initialMd initResp = do
  theBus <-
    liftEffect do
      bus <- create testBus initialMd
      maybeRespond initResp
      pure bus
  senderLoop theBus

  where
  senderLoop :: Bus Atom MbMsg Metadata -> ProcessM SenderRequest Unit
  senderLoop bus = do
    msg <- receive
    case msg.req of
      SetMetadata a -> do
        liftEffect do
          updateMetadata bus a
          maybeRespond msg.resp
        senderLoop bus
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

testBus :: BusRef Atom MbMsg Metadata
testBus = busRef (atom "test-bus")

assertNothing' ∷ ∀ a. String → Maybe a -> Effect Unit
assertNothing' msg =
  case _ of
    Nothing ->
      pure unit
    Just _ -> do
      assertTrue' msg false
