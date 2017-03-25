module Test.ModelTest (tests, tests1,lastTest) where

import Prelude hiding (map)

import Test.HUnit
import Test.QuickCheck
import qualified Data.Map.Strict as M
import qualified Data.List as DL
import Data.These
import Data.Maybe
import Data.Array
import Control.Monad

import Mspiff.Model
import Mspiff.Loader
import Mspiff.Scheduler
import Instances

at :: Assertion
at = assertBool "" True

fs sid = fromJust $ DL.find ((==sid) . screeningId) screenings
s325 = fs 325
s326 = fs 326
s288 = fs 288

tests' :: [Test]
tests' =
    [ "testCyclicOther" ~:
      join (otherScreening <$> (otherScreening s326)) @?= Just s326
    ]

tests :: Test
tests = TestList tests'

tests1 :: Test
tests1 = tests' !! 1

lastTest :: Test
lastTest = last tests'
