module Test.SchedulerTest (tests, tests1,lastTest) where

import Prelude hiding (map)

import Test.HUnit
import qualified Data.List as DL
import qualified Data.Map.Strict as M
import Mspiff.Model
import Mspiff.Scheduler

import Test.TestUtil

s1 :: Screening
s1 = fs 11

o1 :: Screening
o1 = fs (screeningId (overlapping s1 !! 2))

s1' :: Screening
s1' = others s1 !! 0

s0 :: Screening
s0 = fs 126
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
       let (sg:_) = M.elems (addScreening s0 M.empty)
       (screeningId . screening) <$> sg @?= [126]
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
    , "testAddOverlap"  ~: do
       let (sg1:sg2:_) = M.elems (addScreening o1 (addScreening s1 M.empty))
       status <$> sg1 @?= [Scheduled, OtherScheduled, OtherScheduled]
       status <$> sg2 @?= [OtherScheduled, OtherScheduled]
    , "testRuleOut0"  ~: do
       let (sg:_) = M.elems (ruleOutScreening s0 (addScreening s0 M.empty))
       (screeningId . screening) <$> sg @?= [126]
       status <$> sg @?= [RuledOut]
    , "testAddRuleOut"  ~: do
       let (sg:_) = M.elems (ruleOutScreening s0 (addScreening s0 M.empty))
       (screeningId . screening) <$> sg @?= [126]
       status <$> sg @?= [RuledOut]
    , "testAdd0Twice"  ~: do
       let (sg:_) = M.elems (addScreening s0 (addScreening s0 M.empty))
       (screeningId . screening) <$> sg @?= [126]
       status <$> sg @?= [Scheduled]
    , "testAdd<"  ~: do
       let (sg:_) = M.elems (addScreening s1 M.empty)
       (screeningId . screening) <$> sg @?= [11,38]
       status <$> sg @?= [Scheduled, OtherScheduled]
    , "testAdd>"  ~: do
       let (sg:_) = M.elems (addScreening s1' M.empty)
       DL.length sg @?= 2
       status <$> sg @?= [OtherScheduled, Scheduled]
       (screeningId . screening) <$> sg @?= [11,38]
    , "testAdd<<"  ~: do
       let (sg:_) = M.elems (addScreening s1 (addScreening s1 M.empty))
       status <$> sg @?= [Scheduled, OtherScheduled]
       (screeningId . screening) <$> sg @?= [11,38]
    , "testAdd>>"  ~: do
       let (sg:_) = M.elems (addScreening s1' M.empty)
       status <$> sg @?= [OtherScheduled, Scheduled]
       (screeningId . screening) <$> sg @?= [11,38]
    , "testAdd<>"  ~: do
       let (sg:_) = M.elems (addScreening s1' M.empty)
       status <$> sg @?= [OtherScheduled, Scheduled]
       (screeningId . screening) <$> sg @?= [11,38]
    , "testAdd><"  ~: do
       let (sg:_) = M.elems (addScreening s1' (addScreening s1 M.empty))
       status <$> sg @?= [OtherScheduled, Scheduled]
       (screeningId . screening) <$> sg @?= [11,38]
    , "testAdd><"  ~: do
       let (sg1:sg2:_) = M.elems (addScreening s1 (addScreening s0 M.empty))
       status <$> sg1 @?= [Scheduled]
       (screeningId . screening) <$> sg1 @?= [126]
       status <$> sg2 @?= [Scheduled, OtherScheduled]
       (screeningId . screening) <$> sg2 @?= [11,38]
    , "testRuleOut<"  ~: do
       let (sg1:_) = M.elems (ruleOutScreening s1 (addScreening s1 M.empty))
       status <$> sg1 @?= [RuledOut, OtherScheduled]
       pinned <$> sg1 @?= [Unpinned, Unpinned]
       (screeningId . screening) <$> sg1 @?= [11,38]
    , "testRuleOut>"  ~: do
       let (sg1:_) = M.elems (ruleOutScreening s1' (addScreening s1' M.empty))
       status <$> sg1 @?= [OtherScheduled, RuledOut]
       pinned <$> sg1 @?= [Unpinned, Unpinned]
       (screeningId . screening) <$> sg1 @?= [11,38]
    , "testRuleOutBoth"  ~: do
       let (sg1:_) = M.elems (ruleOutScreening s1 (ruleOutScreening s1' (addScreening s1 M.empty)))
       status <$> sg1 @?= [RuledOut, RuledOut]
       pinned <$> sg1 @?= [Unpinned, Unpinned]
       (screeningId . screening) <$> sg1 @?= [11,38]
    , "testRuleOutBoth'"  ~: do
       let (sg1:_) = M.elems (ruleOutScreening s1' (ruleOutScreening s1 (addScreening s1 M.empty)))
       status <$> sg1 @?= [RuledOut, RuledOut]
       pinned <$> sg1 @?= [Unpinned, Unpinned]
       (screeningId . screening) <$> sg1 @?= [11,38]
    , "testRuleOutReadd'"  ~: do
       let (sg1:_) = M.elems (addScreening s1 (ruleOutScreening s1 (addScreening s1 M.empty)))
       status <$> sg1 @?= [Scheduled, OtherScheduled]
       pinned <$> sg1 @?= [Unpinned, Unpinned]
       (screeningId . screening) <$> sg1 @?= [11,38]
    , "testAddOverlapPinReAdd"  ~: do
       let (sg1:sg2:_) = M.elems (addScreening s1 (pinScreening o1 (addScreening o1 (addScreening s1 M.empty))))
       status <$> sg1 @?= [Scheduled, OtherPinned, OtherPinned]
       status <$> sg2 @?= [OtherScheduled, OtherScheduled]
       pinned <$> sg1 @?= [Pinned,Unpinned, Unpinned]
       pinned <$> sg2 @?= [Unpinned, Unpinned]
    , "testAdd2RuleOut1"  ~: do
       let (sg1:sg2:_) = M.elems (addScreening o1 (ruleOutScreening s1 (addScreening o1 (addScreening s1 M.empty))))
       status <$> sg1 @?= [Scheduled, OtherScheduled, OtherScheduled]
       status <$> sg2 @?= [RuledOut, OtherScheduled]
       pinned <$> sg1 @?= [Unpinned, Unpinned, Unpinned]
       pinned <$> sg2 @?= [Unpinned, Unpinned]
    , "testAdd2RuleOut1Pin2"  ~: do
       let (sg1:sg2:_) = M.elems (pinScreening o1 (ruleOutScreening s1 (addScreening o1 (addScreening s1 M.empty))))
       status <$> sg1 @?= [Scheduled, OtherPinned, OtherPinned]
       status <$> sg2 @?= [RuledOut, OtherScheduled]
       pinned <$> sg1 @?= [Pinned, Unpinned, Unpinned]
       pinned <$> sg2 @?= [Unpinned, Unpinned]
    , "testPin<"  ~: do
       let (sg1:_) = M.elems (pinScreening s1 (addScreening s1 M.empty))
       status <$> sg1 @?= [Scheduled, OtherPinned]
       pinned <$> sg1 @?= [Pinned, Unpinned]
       (screeningId . screening) <$> sg1 @?= [11,38]
    , "testPin>"  ~: do
       let (sg1:_) = M.elems (pinScreening s1' (addScreening s1' M.empty))
       status <$> sg1 @?= [OtherPinned, Scheduled]
       pinned <$> sg1 @?= [Unpinned, Pinned]
       (screeningId . screening) <$> sg1 @?= [11,38]
    , "testPinUnpin"  ~: do
       let (sg1:_) = M.elems (unPinScreening s1 (pinScreening s1 (addScreening s1 M.empty)))
       status <$> sg1 @?= [OtherScheduled, OtherScheduled]
       pinned <$> sg1 @?= [Unpinned, Unpinned]
    , "testPinOther"  ~: do
       let (sg1:_) = M.elems (pinScreening s1' (addScreening s1 M.empty))
       status <$> sg1 @?= [OtherPinned, Scheduled]
       pinned <$> sg1 @?= [Unpinned, Pinned]
    , "testPinWithOverlapping" ~: do
       let (sg1:sg2:_) = M.elems (addScreening o1 (pinScreening s1 (addScreening s1 M.empty)))
       status <$> sg1 @?= [OtherScheduled, OtherScheduled, OtherScheduled]
       pinned <$> sg1 @?= [Unpinned, Unpinned, Unpinned]
       status <$> sg2 @?= [Scheduled, OtherPinned]
       pinned <$> sg2 @?= [Pinned, Unpinned]
    , "testPinUnpinOverlapping" ~: do
       let (sg1:sg2:_) = M.elems (pinScreening s1 (pinScreening o1 (addScreening o1 (addScreening s1 M.empty))))
       status <$> sg1 @?= [OtherScheduled, OtherScheduled, OtherScheduled]
       pinned <$> sg1 @?= [Unpinned, Unpinned, Unpinned]
       status <$> sg2 @?= [Scheduled, OtherPinned]
       pinned <$> sg2 @?= [Pinned, Unpinned]
    , "testScheduleOne" ~: do
       let
         st = addScreening s0 M.empty
         (st',Just (Schedule [a]),_) = viewableScheduleFor catalog st
       st' @?= st
       [a] @?= [s0]
    , "testScheduleTwo" ~: do
       let
         st = addScreening s1 (addScreening s0 M.empty)
         (st',Just (Schedule [a,b]),_) = viewableScheduleFor catalog st
       st' @?= st
       print b
       [b,a] @?= [s1,s0]
    , "testScheduleFourOverlapping" ~: do
       let
         sids = [397,398,399,400]
         st = foldr (\sid st_ -> addScreening (fs sid) st_) M.empty sids
         (st',Just (Schedule s),_) = viewableScheduleFor catalog st
       st' @?= st
       print (screeningId <$> s)
    , "testScheduleFourOverlappingWithOneRuledOut" ~: do
       let
         sids = [397,398,399,400]
         st = foldr (\sid st_ -> addScreening (fs sid) st_) M.empty sids
         st' = ruleOutScreening (fs 397) st
         (st'',Just (Schedule s),_) = viewableScheduleFor catalog st'
       st'' @?= st'
       print (s)

    ]

tests :: Test
tests = TestList tests'

tests1 :: Int -> Test
tests1 = (tests' !!)

lastTest :: Test
lastTest = last tests'
