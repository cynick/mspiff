
module Mspiff.Scheduler where

import Prelude

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as M
import Data.Aeson hiding (Array)
import Data.List
import Data.List.Split
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.List as DL
import Data.Maybe
import Data.Array
import Data.These
import Control.Monad
import System.Environment
import System.IO.Unsafe

import Mspiff.Model
import Mspiff.Loader

type Pair a = These a a
type ScreeningPair = Pair Screening
type MarkedScreeningPair = Pair MarkedScreening

mkScreeningPair :: Screening -> Maybe Screening -> ScreeningPair
mkScreeningPair = go
  where
    go a Nothing = This a
    go a (Just b) =
      case a `compare` b of
        LT -> These a b
        GT -> These b a
        EQ -> error "attempt to construct screening pair with a single film"


first :: MarkedScreeningPair -> MarkedScreening
first sp = fromJust (fst <$> justThese sp)

second :: MarkedScreeningPair -> MarkedScreening
second sp = fromJust (snd <$> justThese sp)

isFirst :: Screening -> ScreeningPair -> Bool
isFirst s sp = isThese sp && (fst <$> justThese sp) == Just s

isSecond :: Screening -> ScreeningPair -> Bool
isSecond s sp = isThese sp && (snd <$> justThese sp) == Just s

keyFor :: Screening -> ScreeningPair
keyFor s = mkScreeningPair s (otherScreening s)

mkMsp ::
  (t2 -> t1 -> t) ->
  Screening ->
  (Screening -> t2) ->
  (Screening -> t1) -> t
mkMsp f s ms ms' = f (ms s) (ms' (fromJust (otherScreening s)))

type ScheduleState = M.Map ScreeningPair MarkedScreeningPair

addScreening ::
  Screening ->
  ScheduleState ->
  ScheduleState
addScreening s st =
  let
    k = keyFor s
    ms = MarkedScreening Scheduled Unpinned
    ms' = MarkedScreening OtherScheduled Unpinned
    v | isThis k = This (ms s)
      | isFirst s k = mkMsp These s ms ms'
      | otherwise = mkMsp (flip These) s ms ms'
  in M.insert k v st

ruleOutScreening ::
  Screening ->
  ScheduleState ->
  ScheduleState
ruleOutScreening s st =
  let
    k = keyFor s
    ms = MarkedScreening RuledOut Unpinned
    ms' = MarkedScreening Scheduled Pinned
    v | isThis k = This (ms s)
      | isFirst s k = mkMsp These s ms ms'
      | otherwise = mkMsp (flip These) s ms ms'
    -- If the other screening was already ruled out, we leave it that
    -- way.
    f new old
      | isFirst s k && isRuledOut (second old) =
          let s' = screening (second old) in These (ms s) (ms s')
      | isSecond s k && isRuledOut (first old) =
          let s' = screening (first old) in These (ms s') (ms s)
      | otherwise = new
  in M.insertWith f k v st

removeFilm :: Screening -> ScheduleState -> ScheduleState
removeFilm = M.delete . keyFor

pinScreening ::
  Screening ->
  ScheduleState ->
  ScheduleState
pinScreening s st =
  let
    k = keyFor s
    ms = MarkedScreening Scheduled Pinned
    ms' = MarkedScreening RuledOut Unpinned
    v | isThis k = This (ms s)
      | isFirst s k = mkMsp These s ms ms'
      | otherwise = mkMsp (flip These) s ms ms'
  in M.insert k v st

unPinScreening ::
  Screening ->
  ScheduleState ->
  ScheduleState
unPinScreening = addScreening

{-
updateSchedule ::
  Catalog ->
  WholeSchedule ->
  ScheduleState -> ScheduleState
updateSchedule cat ws st = ret
  where
    schedule = viewableScheduleFor ws ms
    missed = maybe [] (filmsMissedBy cat ws) schedule
    screeningsOfMissed = concatMap filmScreenings missed
    ret = maybe ms rebuild schedule
    rebuild ms = DL.foldl' update [] mss
-}


viewableScheduleFor ::
  WholeSchedule ->
  ScheduleState ->
  (ScheduleState, Maybe ViewableSchedule)
viewableScheduleFor ws st = (st, Schedule <$> schedule)
  where
    schedule = listToMaybe schedules
    schedules = filter (not . null) . filter disjoint . sequence $ lists
    screeningListFor msp ss = ret : ss
      where
        ret =
          case msp of
            This ms -> [screening ms | status ms == Scheduled]
            These ms1 ms2 -> handle ms1 ms2
            _ -> []
        schedulable = [Scheduled,OtherScheduled]
        handle (MarkedScreening st1 _ s1) (MarkedScreening st2 _ s2)
          | st1 `elem` schedulable && st2 `elem` schedulable = [s1,s2]
          | st1 `elem` schedulable = [s1]
          | st2 `elem` schedulable = [s2]
          | otherwise = []
    lists = foldr screeningListFor [] (M.elems st)


viewableSchedulesFor :: WholeSchedule -> [Film] -> [ViewableSchedule]
viewableSchedulesFor ws fs =
  map Schedule .
  filter (not . null) .
  filter disjoint .
  sequence $ screeningListsFor ws fs

viewableSchedulesFor' :: WholeSchedule -> [Film] -> [ViewableSchedule]
viewableSchedulesFor' ws fs = map Schedule $ filter (not.null) $ DL.concat $ reduce start
  where
    f = DL.foldr ((:) . g) []
    g :: [[[Screening]]] -> [[Screening]]
    g = filter (not . null) . fmap stitch . sequence
    stitch [x] = if disjoint x then x else []
    stitch [x,y] =
      let r = (x++y)
      in if disjointLists x y
           then r
           else []
    stitch _ = error "expected to be stitching only lists of one or two elements"
    start = (filter disjoint . sequence) <$> chunksOf 2 (screeningListsFor ws fs)
    reduce :: [[[Screening]]] -> [[[Screening]]]
    reduce [] = []
    reduce [x] = [x]
    reduce xs = reduce (f (chunksOf 2 xs))

filmsInSchedule :: Catalog -> ViewableSchedule -> [Film]
filmsInSchedule cat (Schedule s) =
  catMaybes $ flip lookup fps <$> (scFilmId <$> s)
    where
      fps = zip (filmId <$> cat) cat

filmsNotInSchedule :: Catalog -> ViewableSchedule -> [Film]
filmsNotInSchedule cat vs = cat \\ filmsInSchedule cat vs

filmMissedBy :: WholeSchedule -> ViewableSchedule -> Film -> Bool
filmMissedBy ws (Schedule vs) film = all (not . disjoint) augmentedSchedules
  where
    augmentedSchedules = (:vs) `map` screeningsFor ws film

filmsMissedBy :: Catalog -> WholeSchedule -> ViewableSchedule -> [Film]
filmsMissedBy cat ws vs =
  filter (filmMissedBy ws vs) (filmsNotInSchedule cat vs)

screeningListsFor :: WholeSchedule -> [Film] -> [[Screening]]
screeningListsFor = map . screeningsFor

pairsOf' s = [(x,y) | (x:xs) <- tails s, y <- xs]
pairsOf s = [(a,b) | a <- s, b <- s, a/=b]

disjoint :: [Screening] -> Bool
disjoint = not . any overlaps . pairsOf
  where
    pairsOf s = [(a,b) | a <- s, b <- s, a/=b]

disjointLists' :: [Screening] -> [Screening] -> Bool
disjointLists' x y = disjoint (x++y)


disjointLists :: [Screening] -> [Screening] -> Bool
disjointLists = go
  where
    anyOverlap [] _ = False
    anyOverlap (y:ys) x | x `elem` overlapping y = True
                        | otherwise = anyOverlap ys x
    go [] [] = True
    go _ [] = True
    go [] _ = True
    go x y = not (foldr (\a b -> b || anyOverlap x a) False y)


{-
sequence' :: OverlapMatrix -> [[Screening]] -> [[Screening]]
sequence' mat = foldr lift' ([[]])
  where
    lift' l acc = do
      a <- l
      b <- acc
      guard (g a b)
      return (a:b)
    g a (b:_) = mat ! (screeningId a,screeningId b)
    g _ _ = True
-}
  
{--      a <- l
      b <- acc
      return ((:) a b)
--}   
-- liftM2 f m1 m2          = do { x1 <- m1; x2 <- m2; return (f x1 x2) }

{-
sequence = do
  a <- x
  b <- y
  c <- z
  guard (disjoint [a,b,c])
  return [a,b,c]
  -}

showtimesForSchedule :: ViewableSchedule -> [UTCTime]
showtimesForSchedule = (toUtc <$>) . DL.sort . scheduleScreenings
  where
    toUtc = posixSecondsToUTCTime . fromIntegral . showtime

readInt :: String -> Int
readInt = read

{-
type OverlapMatrix = Array (Int,Int) Bool
overlapMatrix :: [Screening] -> OverlapMatrix
overlapMatrix ss =
  array ((0,0),(len,len))
        [ ((screeningId a,screeningId b), a `overlaps` b)
        | a<- ss, b <- ss
        ]
  where
    len = length ss -1
    overlaps a b = not (a `after` b || b `after` a)

disjoint'' :: [Screening] -> Bool
disjoint'' s =
  (len * (len - 1)) == length [() | a <- s, b <- s, not (a `overlaps` b)]
  where
    len = length s
    overlaps a b = not (a `after` b || b `after` a)

disjoint' :: Array (Int,Int) Bool -> [Screening] -> Bool
disjoint' mat = not . any overlaps . pairsOf
  where
    overlaps (a,b) = mat ! (screeningId a, screeningId b)
    pairsOf s = [(a,b) | a <- s, b <- s, a/=b]


-}

makeHoles :: Eq a => [a] -> [[a]]
makeHoles xs = fmap (\x -> xs \\ [x]) xs

impossiblePairs :: WholeSchedule -> [Film] -> [[Film]]
impossiblePairs w fs = DL.foldr filt [] combos
  where
    combos = [[a,b] | a <- fs, b <- fs, filmId a < filmId b]
    filt a b = if DL.null (viewableSchedulesFor' w a) then a:b else b

impossibleTriples :: WholeSchedule -> [Film] -> [[Film]]
impossibleTriples w fs = DL.foldr filt [] combos
  where
    combos = [[a,b] | a <- fs, b <- fs, c <- fs, filmId a < filmId b && filmId b < filmId c]
    filt a b = if DL.null (viewableSchedulesFor' w a) then a:b else b

