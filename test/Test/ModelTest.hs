module Test.ModelTest (tests, tests1,lastTest) where

import Prelude hiding (map)

import Test.HUnit

import qualified Data.List as DL

import Mspiff.Model

import Test.TestUtil

s2,s3,s4,s5 :: [Screening]
s2 = fs <$> [11,38]
s3 = fs <$> [7,150,319]
s4 = fs <$> [40,146,222,348]
s5 = fs <$> [183,211,251,280,303]

testCyclicOther :: [Screening] -> IO ()
testCyclicOther xs = do
  let
    sids = screeningId <$> xs
    t s = (screeningId <$> others s) @?= sids DL.\\ [screeningId s]
    t' s = do
      let s' = others s
      mapM_ t s'
  mapM_ t xs
  mapM_ t' xs

tests' :: [Test]
tests' =
    [ "testCyclicOther2" ~: testCyclicOther s2
    , "testCyclicOther3" ~: testCyclicOther s3
    , "testCyclicOther4" ~: testCyclicOther s4
    , "testCyclicOther5" ~: testCyclicOther s5
    ]

tests :: Test
tests = TestList tests'

tests1 :: Int -> Test
tests1 = (tests' !!)

lastTest :: Test
lastTest = last tests'
