module Test.Main where

import Prelude
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Kernel.Application (ensureAllStarted)
import Erl.Test.EUnit as Test
import Test.MetadataBus (mbTests)
import Test.SimpleBus (sbTests)
import Test.StateBus (stbTests)

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

main :: Effect Unit
main = do
  void $ ensureAllStarted $ atom "gproc"
  void $ Test.runTests sbTests
  void $ Test.runTests mbTests
  void $ Test.runTests stbTests
