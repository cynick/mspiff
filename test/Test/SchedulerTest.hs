module Test.SchedulerTest (tests, tests1,lastTest) where

import Prelude hiding (map)

import Test.HUnit
import Test.QuickCheck

import Mspiff.Model
import Mspiff.Loader
import Mspiff.Scheduler

at :: Assertion
at = assertBool "" True

tests' :: [Test]
tests' =
    [ "test0"  ~: at
    ]

tests :: Test
tests = TestList tests'

tests1 :: Test
tests1 = tests' !! 1

lastTest :: Test
lastTest = last tests'
