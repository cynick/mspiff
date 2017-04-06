
module Mspiff.ModelUtil where

import Data.Time
import Data.Maybe
import Data.Time.Clock.POSIX
import qualified Data.List as DL
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M

import Mspiff.Model

screeningsFor :: WholeSchedule -> Film -> [Screening]
screeningsFor s f = DL.sort $ filter (filt f) (scheduleScreenings s)
  where filt film sc = scFilmId sc == filmId film

after :: Screening -> Screening -> Bool
after a b = showtime a > showtime b + duration b

overlaps :: (Screening, Screening) -> Bool
overlaps (a, b) = not (a `after` b || b `after` a)

isRuledOut :: MarkedScreening -> Bool
isRuledOut = (==RuledOut) . status

isPinned :: MarkedScreening -> Bool
isPinned = (==Pinned) . pinned

isOther :: Screening -> Screening -> Bool
isOther s s' = s /= s' && scFilmId s' == scFilmId s

showtimeToUtc :: Screening -> UTCTime
showtimeToUtc = posixSecondsToUTCTime . fromIntegral . showtime

dayOf :: Screening -> Day
dayOf = localDay . zonedTimeToLocalTime . utcToZonedTime tz . showtimeToUtc
  where
    tz = hoursToTimeZone (-5)

toPersistState :: ScheduleState -> PersistState
toPersistState ss = PersistState $ concat ps
  where
    ps = fmap toPs <$> M.elems ss
    toPs MarkedScreening{..} = (screeningId screening, status, pinned)

fromPersistState :: ScreeningMap -> PersistState -> ScheduleState
fromPersistState smap (PersistState ps) =
  M.fromList $ toScreeningGroup <$> groups
  where
    groups =
      NE.groupAllWith (scFilmId . screening) (fromPs <$> ps)
    toScreeningGroup ms =
      (scFilmId . screening $ NE.head ms, NE.toList ms)
    fromPs (sid, status, pinned) =
      MarkedScreening status pinned (fromJust (M.lookup sid smap))
