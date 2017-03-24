module Main where

import Prelude
import Test.HUnit

import qualified Test.ModelTest
import qualified Test.SchedulerTest
import qualified Test.StateTest

tests :: Test
tests = TestList
        [ Test.ModelTest.tests
        , Test.SchedulerTest.tests
        , Test.StateTest.tests
        ]

run :: IO Counts
run = runTestTT $ TestList [tests]

main :: IO Counts
main = run


