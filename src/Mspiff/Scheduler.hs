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
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Exception.Base
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
  (st, schedule, join $ maybeToList filmsMissed)
  where
    schedule = listToMaybe schedules
    filmsMissed = filmsNotInSchedule cat `fmap` schedule
    schedules = viewableSchedulesForScreenings lists
    screeningListFor screeningGroup =
      [screening ms | ms <- screeningGroup, status ms `elem` schedulable]

    lists = screeningListFor <$> M.elems st

viewableSchedulesFor :: [Film] -> [ViewableSchedule]
viewableSchedulesFor fs =
  map Schedule .
  filter (not . null) .
  filter disjoint .
  sequence $ screeningListsFor fs

viewableSchedulesForScreenings :: [[Screening]] -> [ViewableSchedule]
viewableSchedulesForScreenings list =
  map Schedule $ filter (not . null) $ DL.concat $ reduce start
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
    start = (filter disjoint . sequence) <$> chunksOf 2 list
    reduce :: [[[Screening]]] -> [[[Screening]]]
    reduce [] = []
    reduce [x] = [x]
    reduce xs = reduce (f (chunksOf 2 xs))

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

updateState :: ScheduleState -> Command -> ScheduleState
updateState st = go
  where
    go cmd =
      case cmd of
        Add s -> addScreening s st
        RuleOut s -> ruleOutScreening s st
        Pin s -> pinScreening s st
        UnPin s -> unPinScreening s st
        RemoveFilm s -> removeFilm s st
        Clear -> M.empty
        _ -> st

data LoopState = LoopState
  { st :: ScheduleState
  , tid :: Maybe ThreadId
  , tvar :: TMVar ScheduleState
  }

runScheduler ::
  Catalog ->
  (ScheduleState -> ScheduleState -> IO ()) ->
  (IO (), IO ()) ->
  TMVar ScheduleState ->
  IO ()
runScheduler cat redraw startEnd tvar =
  uncurry bracket_ startEnd $ do
    (st,st') <- atomically $ do
      st <- takeTMVar tvar
      let
        (st', vs, missed) = viewableScheduleFor cat st
        st'' = maybe st' (foldr addScreening st' . scheduleScreenings) vs
      putTMVar tvar st''
      return (st, st'')
    redraw st st'

processCommand ::
  (Monad m, MonadIO m) =>
  Catalog ->
  (ScheduleState -> ScheduleState -> IO ()) ->
  (IO (), IO ()) ->
  LoopState ->
  C.ConduitM Command Void m ()
processCommand cat redraw startEnd orig = C.evalStateC orig $ C.awaitForever $ \cmd -> do
  LoopState{..} <- get
  {- At this point, there are two scenarios.
   a) A scheduler thread is running.
        We kill it, as any result it comes up with will be stale.
   b) A scheduler thread is *not* running.
        We check the tvar for an update ScheduleState
        If there's something there, we use it as the argument to updateState.
  -}
  let
    maybeKillThread = liftIO . maybe (return ()) killThread
  maybeKillThread tid
  st'' <- liftIO $ atomically $ do
    st' <- fromMaybe st <$> tryTakeTMVar tvar
    let new = updateState st' cmd
    putTMVar tvar new
    return new
  liftIO $ redraw st st''
  tid' <- liftIO $ forkIO $ runScheduler cat redraw startEnd tvar
  put (LoopState st'' (Just tid') tvar)

startSchedulerLoop ::
  TBMChan Command ->
  Catalog ->
  (ScheduleState -> ScheduleState -> IO ()) ->
  (IO (), IO ()) ->
  ScheduleState ->
  IO ThreadId
startSchedulerLoop chan catalog redraw startEnd orig = do
  tvar <- newEmptyTMVarIO
  let st = LoopState orig Nothing tvar
  forkIO $ sourceTBMChan chan $$ processCommand catalog redraw startEnd st


