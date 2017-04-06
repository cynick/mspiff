module Mspiff.Scheduler where

import Prelude

import qualified Data.Map.Strict as M
import Data.List
import Data.List.Split
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.List as DL
import Data.Maybe
import Data.Void
import Data.Conduit.TMChan
import Control.Concurrent
import Control.Monad.State
import Data.Conduit as C
import qualified Data.Conduit.Lift as C

import Mspiff.Model
import Mspiff.ModelUtil

type ScreeningGroup = [MarkedScreening]
mkScreeningGroup ::
  Screening ->
  (Screening -> MarkedScreening) ->
  (Screening -> MarkedScreening) -> ScreeningGroup
mkScreeningGroup s ms ms' =
  DL.sort (ms s : (ms' <$> others s))

addScreening ::
  Screening ->
  ScheduleState ->
  ScheduleState
addScreening s = M.insert k v
  where
    (k,v) = (scFilmId s, mkScreeningGroup s ms ms')
    ms = MarkedScreening Scheduled Unpinned
    ms' = MarkedScreening OtherScheduled Unpinned

ruleOutScreening ::
  Screening ->
  ScheduleState ->
  ScheduleState
ruleOutScreening s = M.insertWith f k v
  where
    (k,v) = (scFilmId s, mkScreeningGroup s ms ms')
    ms = MarkedScreening RuledOut Unpinned
    ms' = MarkedScreening Scheduled Unpinned

    -- If any other screening was already ruled out, we leave it that
    -- way.

    merge [] _ a = DL.sort a
    merge _ [] a = DL.sort a
    merge (x:xs) (y:ys) a
      | screening x == s = merge xs ys (ms s : a)
      | isRuledOut y = merge xs ys (y : a)
      | otherwise = merge xs ys (ms' (screening y) : a)
    f new old = merge new old []

removeFilm :: Screening -> ScheduleState -> ScheduleState
removeFilm = M.delete . scFilmId

pinScreening ::
  Screening ->
  ScheduleState ->
  ScheduleState
pinScreening s = M.insert k v
  where
    (k,v) = (scFilmId s, mkScreeningGroup s ms ms')
    ms = MarkedScreening Scheduled Pinned
    ms' = MarkedScreening RuledOut Unpinned

unPinScreening ::
  Screening ->
  ScheduleState ->
  ScheduleState
unPinScreening = addScreening

schedulable :: [ScreeningStatus]
schedulable = [Scheduled,OtherScheduled]

viewableScheduleFor ::
  Catalog ->
  ScheduleState ->
  (ScheduleState, Maybe ViewableSchedule, [Film])
viewableScheduleFor cat st =
  (st, Schedule <$> schedule, join $ maybeToList filmsMissed)
  where
    schedule = listToMaybe schedules
    filmsMissed = filmsNotInSchedule cat `fmap` schedule
    schedules = filter (not . null) . filter disjoint . sequence $ lists
    screeningListFor screeningGroup =
      [screening ms | ms <- screeningGroup, status ms `elem` schedulable]

    lists = screeningListFor <$> M.elems st

viewableSchedulesFor :: [Film] -> [ViewableSchedule]
viewableSchedulesFor fs =
  map Schedule .
  filter (not . null) .
  filter disjoint .
  sequence $ screeningListsFor fs

viewableSchedulesFor' :: [Film] -> [ViewableSchedule]
viewableSchedulesFor' fs = map Schedule $ filter (not.null) $ DL.concat $ reduce start
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
    start = (filter disjoint . sequence) <$> chunksOf 2 (screeningListsFor fs)
    reduce :: [[[Screening]]] -> [[[Screening]]]
    reduce [] = []
    reduce [x] = [x]
    reduce xs = reduce (f (chunksOf 2 xs))

filmsInSchedule :: Catalog -> [Screening] -> [Film]
filmsInSchedule Catalog{..} ss =
  catMaybes $ flip M.lookup filmMap <$> DL.nub (scFilmId <$> ss)
    where
      fps = zip (filmId <$> films) films

filmsNotInSchedule :: Catalog -> [Screening] -> [Film]
filmsNotInSchedule cat@Catalog{..} ss = films \\ filmsInSchedule cat ss

filmMissedBy :: [Screening] -> Film -> Bool
filmMissedBy ss film = all (not . disjoint) augmentedSchedules
  where
    augmentedSchedules = (:ss) `map` filmScreenings film

filmsMissedBy :: Catalog -> [Screening] -> [Film]
filmsMissedBy cat@Catalog{..} ss =
  filter (filmMissedBy ss) (filmsNotInSchedule cat ss)

screeningListsFor :: [Film] -> [[Screening]]
screeningListsFor = (filmScreenings <$>)

pairsOf' :: [t] -> [(t, t)]
pairsOf' s = [(x,y) | (x:xs) <- tails s, y <- xs]

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

showtimesForSchedule :: ViewableSchedule -> [UTCTime]
showtimesForSchedule = (toUtc <$>) . DL.sort . scheduleScreenings
  where
    toUtc = posixSecondsToUTCTime . fromIntegral . showtime

readInt :: String -> Int
readInt = read

makeHoles :: Eq a => [a] -> [[a]]
makeHoles xs = fmap (\x -> xs \\ [x]) xs

impossiblePairs :: [Film] -> [[Film]]
impossiblePairs fs = DL.foldr filt [] combos
  where
    combos = [[a,b] | a <- fs, b <- fs, filmId a < filmId b]
    filt a b = if DL.null (viewableSchedulesFor' a) then a:b else b

impossibleTriples :: [Film] -> [[Film]]
impossibleTriples fs = DL.foldr filt [] combos
  where
    combos = [[a,b] | a <- fs, b <- fs, c <- fs, filmId a < filmId b && filmId b < filmId c]
    filt a b = if DL.null (viewableSchedulesFor' a) then a:b else b

runCmd :: Catalog -> ScheduleState -> Command -> ScheduleState
runCmd cat@Catalog{..} st = schedule . run
  where
    run cmd =
      case cmd of
        Add s -> addScreening s st
        RuleOut s -> ruleOutScreening s st
        Pin s -> pinScreening s st
        UnPin s -> unPinScreening s st
        RemoveFilm s -> removeFilm s st
        Clear -> M.empty
    schedule st =
      maybe st' (foldr addScreening st' . scheduleScreenings) vs
      where
        (st', vs, missed) = viewableScheduleFor cat st

updateState ::
  (Monad m, MonadIO m) =>
  Catalog ->
  (ScheduleState -> ScheduleState -> IO ()) ->
  ScheduleState ->
  C.ConduitM Command Void m ()
updateState catalog update orig = C.evalStateC orig $ C.awaitForever $ \cmd -> do
  st <- get
  let st' = runCmd catalog st cmd
  liftIO $ update st st'
  put st'

startSchedulerLoop ::
  TBMChan Command ->
  Catalog ->
  (ScheduleState -> ScheduleState -> IO ()) ->
  ScheduleState ->
  IO ThreadId
startSchedulerLoop chan catalog update orig =
  forkIO $ sourceTBMChan chan $$ updateState catalog update orig


