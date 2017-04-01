module Test.SchedulerTest (tests, tests1,lastTest) where

import Prelude hiding (map)

import Test.HUnit
import qualified Data.List as DL
import qualified Data.Map.Strict as M
import Mspiff.Model
import Mspiff.Scheduler

import Test.TestUtil

w :: WholeSchedule
w = Schedule screenings_

s1 :: Screening
s1 = fs 11

s1' :: Screening
s1' = others s1 !! 0

s0 :: Screening
s0 = fs 248
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
       let (ScreeningGroup sg:_) = M.elems (addScreening s0 M.empty)
       (screeningId . screening) <$> sg @?= [248]
       status <$> sg @?= [Scheduled]

    , "testAddRemove0"  ~: do
       let s = removeFilm s0 (addScreening s0 M.empty)
       DL.length s @?= 0
    , "testAddRemove1"  ~: do
       let s = removeFilm s1 (addScreening s1 M.empty)
       DL.length s @?= 0
    , "testAddRemove2"  ~: do
       let s = removeFilm s0 (addScreening s1 (addScreening s0 M.empty))
       DL.length s @?= 1
    , "testRuleOut0"  ~: do
       let (ScreeningGroup sg:_) = M.elems (ruleOutScreening s0 M.empty)
       (screeningId . screening) <$> sg @?= [248]
       status <$> sg @?= [RuledOut]
    , "testAddRuleOut"  ~: do
       let (ScreeningGroup sg:_) = M.elems (ruleOutScreening s0 (addScreening s0 M.empty))
       (screeningId . screening) <$> sg @?= [248]
       status <$> sg @?= [RuledOut]
    , "testAdd0Twice"  ~: do
       let (ScreeningGroup sg:_) = M.elems (addScreening s0 (addScreening s0 M.empty))
       (screeningId . screening) <$> sg @?= [248]
       status <$> sg @?= [Scheduled]
    , "testAdd<"  ~: do
       let (ScreeningGroup sg:_) = M.elems (addScreening s1 M.empty)
       (screeningId . screening) <$> sg @?= [11,38]
       status <$> sg @?= [Scheduled, OtherScheduled]
    , "testAdd>"  ~: do
       let (ScreeningGroup sg:_) = M.elems (addScreening s1' M.empty)
       DL.length sg @?= 2
       status <$> sg @?= [OtherScheduled, Scheduled]
       (screeningId . screening) <$> sg @?= [325, 200]
    , "testAdd<<"  ~: do
       let (ScreeningGroup sg:_) = M.elems (addScreening s1 (addScreening s1 M.empty))
       status <$> sg @?= [Scheduled, OtherScheduled]
       (screeningId . screening) <$> sg @?= [325, 200]
    , "testAdd>>"  ~: do
       let (ScreeningGroup sg:_) = M.elems (addScreening s1' M.empty)
       status <$> sg @?= [OtherScheduled, Scheduled]
       (screeningId . screening) <$> sg @?= [325, 200]
    , "testAdd<>"  ~: do
       let (ScreeningGroup sg:_) = M.elems (addScreening s1' M.empty)
       status <$> sg @?= [OtherScheduled, Scheduled]
       (screeningId . screening) <$> sg @?= [325, 200]
    , "testAdd><"  ~: do
       let (ScreeningGroup sg:_) = M.elems (addScreening s1 (addScreening s1' M.empty))
       status <$> sg @?= [Scheduled, OtherScheduled]
       (screeningId . screening) <$> sg @?= [325, 200]
    , "testAdd><"  ~: do
       let (ScreeningGroup sg1:ScreeningGroup sg2:_) = M.elems (addScreening s1 (addScreening s0 M.empty))
       status <$> sg1 @?= [Scheduled]
       (screeningId . screening) <$> sg1 @?= [248]
       status <$> sg2 @?= [Scheduled, OtherScheduled]
       (screeningId . screening) <$> sg2 @?= [325, 200]
    , "testRuleOut<"  ~: do
       let (ScreeningGroup sg1:_) = M.elems (ruleOutScreening s1 (addScreening s1 M.empty))
       status <$> sg1 @?= [RuledOut, Scheduled]
       pinned <$> sg1 @?= [Unpinned, Pinned]
       (screeningId . screening) <$> sg1 @?= [325, 200]
    , "testRuleOut>"  ~: do
       let (ScreeningGroup sg1:_) = M.elems (ruleOutScreening s1' (addScreening s1' M.empty))
       status <$> sg1 @?= [Scheduled, RuledOut]
       pinned <$> sg1 @?= [Pinned, Unpinned]
       (screeningId . screening) <$> sg1 @?= [325, 200]
    , "testRuleOutBoth"  ~: do
       let (ScreeningGroup sg1:_) = M.elems (ruleOutScreening s1 (ruleOutScreening s1' (addScreening s1 M.empty)))
       status <$> sg1 @?= [RuledOut, RuledOut]
       pinned <$> sg1 @?= [Unpinned, Unpinned]
       (screeningId . screening) <$> sg1 @?= [325, 200]
    , "testPin<"  ~: do
       let (ScreeningGroup sg1:_) = M.elems (pinScreening s1 (addScreening s1 M.empty))
       status <$> sg1 @?= [Scheduled, RuledOut]
       pinned <$> sg1 @?= [Pinned, Unpinned]
       (screeningId . screening) <$> sg1 @?= [325, 200]
    , "testPin>"  ~: do
       let (ScreeningGroup sg1:_) = M.elems (pinScreening s1' (addScreening s1' M.empty))
       status <$> sg1 @?= [RuledOut, Scheduled]
       pinned <$> sg1 @?= [Unpinned, Pinned]
       (screeningId . screening) <$> sg1 @?= [100, 200, 325]
    , "testScheduleOne" ~: do
       let
         st = addScreening s0 M.empty
         (st',Just (Schedule [a])) = viewableScheduleFor w st
       st' @?= st
       [a] @?= [s0]
    , "testScheduleTwo" ~: do
       let
         st = addScreening s1 (addScreening s0 M.empty)
         (st',Just (Schedule [a,b])) = viewableScheduleFor w st
       st' @?= st
       print b
       [b,a] @?= [s1,s0]
    , "testScheduleFourOverlapping" ~: do
       let
         sids = [293,115,265,57]
         st = foldr (\sid st_ -> addScreening (fs sid) st_) M.empty sids
         (st',Just (Schedule s)) = viewableScheduleFor w st
       st' @?= st
       print s
    , "testScheduleFourOverlappingWithOneRuledOut" ~: do
       let
         sids = [293,115,265,57]
         st = foldr (\sid st_ -> addScreening (fs sid) st_) M.empty sids
         st' = ruleOutScreening (fs 56) st
         (st'',Just (Schedule s)) = viewableScheduleFor w st'
       st'' @?= st'
       print s

    ]

tests :: Test
tests = TestList tests'

tests1 :: Int -> Test
tests1 = (tests' !!)

lastTest :: Test
lastTest = last tests'
