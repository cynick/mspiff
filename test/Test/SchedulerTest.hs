module Test.SchedulerTest (tests, tests1,lastTest) where

import Prelude hiding (map)

import Test.HUnit
import Test.QuickCheck
import qualified Data.Map.Strict as M
import qualified Data.List as DL
import Data.These
import Data.Maybe
import Data.Array

import Mspiff.Model
import Mspiff.Loader
import Mspiff.Scheduler
import TestUtil

at :: Assertion
at = assertBool "" True

nonempty x |DL.null x = False |otherwise = True

w = Schedule screenings

{-    
t' :: FilmList -> Bool
t' (FilmList f) = nonempty (viewableSchedulesFor' w f)
t'' :: DisjointList -> DisjointList -> Bool
t'' (DisjointList s) (DisjointList s') = disjointLists s s' == disjointLists' s s'
-}

mt f = mapThese f f
other = fromJust . otherScreening
s = DL.head $ DL.drop 100 screenings
s' = other s
s1 = addScreening s M.empty
s2 = addScreening s' s1
s3 = pinScreening s s2
s4 = pinScreening (fromJust (otherScreening s)) s3
s5 = ruleOutScreening s s4
(msp1:_) = M.elems s1
(msp2:_) = M.elems s2
(msp3:_) = M.elems s3
(msp4:_) = M.elems s4

tests' :: [Test]
tests' =
    [
    ]

tests :: Test
tests = TestList tests'

tests1 :: Test
tests1 = tests' !! 1

lastTest :: Test
lastTest = last tests'
