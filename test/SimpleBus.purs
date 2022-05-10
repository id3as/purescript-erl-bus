module Test.SimpleBus where

import Prelude
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Erl.Atom (Atom, atom)
import Erl.Kernel.Erlang (sleep)
import Erl.Process (ProcessM, spawnLink, toPid)
import Erl.Process as Process
import Erl.Process.Raw as Raw
import Erl.Test.EUnit as Test
import SimpleBus (Bus, subscribe, raise, bus, testHelpers)
import Test.Assert (assertEqual')

data Msg
  = TestMsg Int
instance Show Timeout where
  show Timeout = "Timeout"

data Timeout
  = Timeout
derive instance Eq Timeout

derive instance Eq Msg
instance Show Msg where
  show (TestMsg i) = "testMsg " <> show i


sbTests :: Test.TestSuite
sbTests = do
  Test.suite "simple-bus tests" do
    canSendAndReceiveMessages
    canSendAndReceiveMappedMessages
    differentMappersPerProcess
    enableDisable

canSendAndReceiveMessages :: Test.TestSuite
canSendAndReceiveMessages = do
  Test.test "Can send and receive messages" do
    me <- Raw.self
    _ <- spawnLink $ process me
    _ <- Raw.receive
    pure unit
  where

  process :: Raw.Pid -> ProcessM Msg Unit
  process parent = do
    let msgSent = TestMsg 1
    subscribe testBus identity
    liftEffect $ raise testBus msgSent
    msg <- Process.receive
    liftEffect do
      assertEqual' "Original message received" { actual: msg, expected: msgSent }
      Raw.send parent unit
    pure unit

canSendAndReceiveMappedMessages :: Test.TestSuite
canSendAndReceiveMappedMessages = do
  Test.test "Can send and receive messages with mapping" do
    me <- Raw.self
    _ <- spawnLink $ process me
    _ <- Raw.receive
    pure unit
  where
  mapper :: Msg -> Msg
  mapper (TestMsg i) = TestMsg (i + 1)

  process :: Raw.Pid -> ProcessM Msg Unit
  process parent = do
    subscribe testBus mapper
    liftEffect $ raise testBus $ TestMsg 1
    msg <- Process.receive
    liftEffect do
      assertEqual' "Mapper is applied" { actual: msg, expected: TestMsg 2 }
      Raw.send parent unit
    pure unit

differentMappersPerProcess :: Test.TestSuite
differentMappersPerProcess = do
  Test.test "Different processes get their own mapper" do
    me <- Raw.self
    _ <- spawnLink $ process me 1
    _ <- spawnLink $ process me 10
    liftEffect do
      sleep (Milliseconds 10.0)
      raise testBus $ TestMsg 1
    _ <- Raw.receive
    _ <- Raw.receive
    pure unit
  where
  mapper :: Int -> Msg -> Msg
  mapper delta (TestMsg i) = TestMsg (i + delta)

  process :: Raw.Pid -> Int -> ProcessM Msg Unit
  process parent delta = do
    subscribe testBus $ mapper delta
    msg <- Process.receive
    liftEffect do
      assertEqual' "Mapper is applied" { actual: msg, expected: TestMsg (1 + delta) }
      Raw.send parent unit
    pure unit

enableDisable :: Test.TestSuite
enableDisable = do
  Test.test "Exercise the testHelper API" do
    me <- Raw.self
    -- spawn the child and wait for it to say it has subscribed (disabled)
    child <- spawnLink $ process me
    _ <- Raw.receive
    liftEffect do
      raise testBus $ TestMsg 1
      -- Wait to confirm the child did NOT receive this (disabled)
      msg1 <- Raw.receiveWithTimeout (Milliseconds 10.0) Timeout
      assertEqual' "No message initially" { actual: msg1, expected: Timeout }
      -- Send child a message for them to enable the bus
      Raw.send (toPid child) $ TestMsg 1000
      sleep (Milliseconds 10.0)
      raise testBus $ TestMsg 2
    -- they should receive that message
    msg2 <- Raw.receive
    assertEqual' "Child received mapped message" { actual: msg2, expected: TestMsg 3 }
    pure unit
  where
  mapper :: Msg -> Msg
  mapper (TestMsg i) = TestMsg (i + 1)

  process :: Raw.Pid -> ProcessM Msg Unit
  process parent = do
    -- Start disabled
    testHelpers.subscribeDisabled testBus mapper
    liftEffect do
      -- Let the parent know we have registered
      Raw.send parent unit
      -- wait for the parent to ask us to continue (after the first raise has not arrived, as we are disabled)
      msg1 <- Raw.receive
      assertEqual' "Continue message from parent" { actual: msg1, expected: TestMsg 1000 }
      testHelpers.enable testBus
    msg2 <- Process.receive
    liftEffect do
      assertEqual' "Mapper is applied" { actual: msg2, expected: TestMsg 3 }
      Raw.send parent msg2
    pure unit

testBus :: Bus Atom Msg
testBus = bus (atom "test-bus")
