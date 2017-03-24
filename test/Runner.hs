module Main where

import Prelude
import Test.HUnit

import qualified Test.SchedulerTest

tests :: Test
tests = TestList
        [
--         Test.SchedulerTest.tests
        ]

main :: IO Counts
main = runTestTT $ TestList [tests]

