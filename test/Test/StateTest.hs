module Test.StateTest (tests, tests1,lastTest) where

import Prelude hiding (map)

import Test.HUnit
import qualified Data.Map.Strict as M
import qualified Data.List as DL
import Data.These
import Data.Maybe

import Mspiff.Model
import Mspiff.Loader
import Mspiff.Scheduler
import TestUtil

at :: Assertion
at = assertBool "" True

mt f = mapThese f f
other = fromJust . otherScreening
s = screenings !! 100
s' = other s

s0 = fs 288
{-
s1 = addScreening s M.empty
s2 = addScreening s' s1
s3 = pinScreening s s2
s4 = pinScreening (fromJust (otherScreening s)) s3
s5 = ruleOutScreening s s4
(msp1:_) = M.elems s1
(msp2:_) = M.elems s2
(msp3:_) = M.elems s3
(msp4:_) = M.elems s4
-}

tests' :: [Test]
tests' =
    [ "test0" ~: at
    , "testAdd0"  ~: do
       let (msp:_) = M.elems (addScreening s0 M.empty)
       mt (screeningId . screening) msp @?= This 288
       mt status msp @?= This Scheduled
    , "testAddRemove0"  ~: do
       let s1 = (removeFilm s0 (addScreening s0 M.empty))
       DL.length s1 @?= 0
    , "testAddRemove1"  ~: do
       let s1 = (removeFilm s (addScreening s M.empty))
       DL.length s1 @?= 0
    , "testAddRemove2"  ~: do
       let s1 = (removeFilm s (addScreening s (addScreening s0 M.empty)))
       DL.length s1 @?= 1
    , "testRuleOut0"  ~: do
       let (msp:_) = M.elems (ruleOutScreening s0 M.empty)
       mt (screeningId . screening) msp @?= This 288
       mt status msp @?= This RuledOut
    , "testAddRuleOut"  ~: do
       let (msp:_) = M.elems (ruleOutScreening s0 (addScreening s0 M.empty))
       mt (screeningId . screening) msp @?= This 288
       mt status msp @?= This RuledOut
    , "testAdd0Twice"  ~: do
       let (msp:_) = M.elems (addScreening s0 (addScreening s0 M.empty))
       mt (screeningId . screening) msp @?= This 288
       mt status msp @?= This Scheduled
    , "testAdd<"  ~: do
       let (msp:_) = M.elems (addScreening s M.empty)
       mt (screeningId . screening) msp @?= These 325 326
       mt status msp @?= These Scheduled OtherScheduled
    , "testAdd>"  ~: do
       let (msp:_) = M.elems (addScreening s' M.empty)
       mt status msp @?= These OtherScheduled Scheduled
       mt (screeningId . screening) msp @?= These 325 326
    , "testAdd<<"  ~: do
       let (msp:_) = M.elems (addScreening s (addScreening s M.empty))
       mt status msp @?= These Scheduled OtherScheduled
       mt (screeningId . screening) msp @?= These 325 326
    , "testAdd>>"  ~: do
       let (msp:_) = M.elems (addScreening s' (addScreening s' M.empty))
       mt status msp @?= These OtherScheduled Scheduled
       mt (screeningId . screening) msp @?= These 325 326
    , "testAdd<>"  ~: do
       let (msp:_) = M.elems (addScreening s' (addScreening s M.empty))
       mt status msp @?= These OtherScheduled Scheduled
       mt (screeningId . screening) msp @?= These 325 326
    , "testAdd><"  ~: do
       let (msp:_) = M.elems (addScreening s (addScreening s' M.empty))
       mt status msp @?= These Scheduled OtherScheduled
       mt (screeningId . screening) msp @?= These 325 326
    , "testAdd><"  ~: do
       let (msp1:msp2:_) = M.elems (addScreening s (addScreening s0 M.empty))
       mt status msp1 @?= This Scheduled
       mt (screeningId . screening) msp1 @?= This 288
       mt status msp2 @?= These Scheduled OtherScheduled
       mt (screeningId . screening) msp2 @?= These 325 326
    ]

tests :: Test
tests = TestList tests'

tests1 :: Int -> Test
tests1 = (tests' !!)

lastTest :: Test
lastTest = last tests'

