module Test.ModelTest (tests, tests1,lastTest) where

import Prelude hiding (map)

import Test.HUnit
import Test.QuickCheck
import qualified Data.Map.Strict as M
import qualified Data.List as DL
import Control.Monad

import Mspiff.Model
import Mspiff.Loader
import Mspiff.Scheduler
import Test.TestUtil

s11 = fs 11

tests' :: [Test]
tests' =
    [ "testCyclicOther" ~:
       join (fmap others <$> others s11) @?= Just [11]
        at
    ]

tests :: Test
tests = TestList tests'

tests1 :: Int -> Test
tests1 = (tests' !!)

lastTest :: Test
lastTest = last tests'
