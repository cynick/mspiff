module Mspiff.Model where

import Prelude
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.List.NonEmpty as NE
import Data.Aeson
import Data.Time
import Data.Time.Clock.POSIX
import Data.Monoid
import Data.Maybe


type VenueId = Int
type FilmId = Int
type ScreeningId = Int
type Title = T.Text
type Showtime = Int
type Duration = Int
type VenueName = T.Text
type Url = T.Text

data Film = Film
  { filmId :: FilmId
  , filmTitle :: Title
  , filmScreenings :: [Screening]
  , filmUrl :: Url
  }
  deriving Show

instance Ord Film where
  a `compare` b = filmTitle a `compare` filmTitle b

instance Eq Film where
  a == b = filmId a == filmId b

instance ToJSON Film where
  toJSON Film{..} =
    object
      [ "filmId" .= filmId
      , "filmTitle" .= filmTitle
      , "filmUrl" .= filmUrl
      ]

instance FromJSON Film where
  parseJSON (Object v) =
    Film
      <$> v .: "filmId"
      <*> v .: "filmTitle"
      <*> pure []
      <*> v .: "filmUrl"

  parseJSON _ = error "invalid film json"

data Venue = Venue
  { venueId :: VenueId
  , venueName :: VenueName
  }
  deriving (Show,Ord,Eq)

instance ToJSON Venue where
  toJSON Venue{..} =
    object [ "id" .= venueId, "name" .= venueName ]

instance FromJSON Venue where
  parseJSON (Object o) =
    Venue
      <$> o .: "id"
      <*> o .: "name"
  parseJSON x = error $ "invalid venue json " ++ show x

data Screening = Screening
  { scFilmId :: FilmId
  , screeningId :: ScreeningId
  , overlapping :: [Screening]
  , others :: [Screening]
  , showtime :: Showtime
  , duration :: Duration
  , scVenueId :: VenueId
  }

instance Show Screening where
  show s =
    "Screening {scFilmId = " <> show (scFilmId s) <>
    ", screeningId = " <> show (screeningId s) <>
    ", overlapping = " <> show (screeningId <$> overlapping s) <>
    ", others = " <> show (screeningId <$> others s) <>
    ", showtime = " <> show (showtime s) <>
    ", duration = " <> show (duration s) <>
    ", venue = " <> show (scVenueId s) <>
    "}"

instance Eq Screening where
  a == b = screeningId a == screeningId b

instance Ord Screening where
  compare a b =
    case showtime a `compare` showtime b of
      LT -> LT
      GT -> GT
      EQ -> scVenueId a `compare` scVenueId b

instance ToJSON Screening where
  toJSON Screening{..} =
    object
      [ "scFilmId" .= scFilmId
      , "screeningId" .= screeningId
      , "showtime" .= showtime
      , "duration" .= duration
      , "venue" .= scVenueId
      ]

instance FromJSON Screening where
  parseJSON (Object v) =
    Screening
     <$> v .: "scFilmId"
     <*> v .: "screeningId"
     <*> pure []
     <*> pure []
     <*> v .: "showtime"  -- seconds since Epoch
     <*> v .: "duration" -- duration is given in minutes
     <*> v .: "venue"

  parseJSON _ = error "invalid screening json"

data Pinned = Pinned | Unpinned
  deriving (Show, Read, Eq, Bounded, Enum)

instance ToJSON Pinned where
  toJSON Pinned = Number 0
  toJSON Unpinned = Number 1

instance FromJSON Pinned where
  parseJSON (Number n) = return $ if n == 0 then Pinned else Unpinned
  parseJSON _ = error "invalid pinned json"

data ScreeningStatus =
  Unscheduled | Scheduled | Impossible | RuledOut | OtherScheduled
  deriving (Show, Read, Eq, Bounded, Enum)

instance ToJSON ScreeningStatus where
  toJSON Unscheduled = Number 0
  toJSON Scheduled = Number 1
  toJSON Impossible = Number 2
  toJSON RuledOut = Number 3
  toJSON OtherScheduled = Number 4

instance FromJSON ScreeningStatus where
  parseJSON (Number n) =
    return $ case n of
      0 -> Unscheduled
      1 -> Scheduled
      2 -> Impossible
      3 -> RuledOut
      4 -> OtherScheduled
      _ -> error "unrecognized screening status"
  parseJSON _ = error "invalid screening status json"

data MarkedScreening = MarkedScreening
  { status :: ScreeningStatus
  , pinned :: Pinned
  , screening :: Screening
  }
  deriving Show

instance Eq MarkedScreening where
  a == b = screening a == screening b

instance Ord MarkedScreening where
  a `compare` b = screening a `compare` screening b


isRuledOut :: MarkedScreening -> Bool
isRuledOut = (==RuledOut) . status

isPinned :: MarkedScreening -> Bool
isPinned = (==Pinned) . pinned

isOther :: Screening -> Screening -> Bool
isOther s s' = s /= s' && scFilmId s' == scFilmId s

newtype Schedule = Schedule { scheduleScreenings :: [Screening] }
  deriving (Eq,Show,Monoid)

type WholeSchedule = Schedule
type VenueSchedule = Schedule
type ViewableSchedule = Schedule

showtimeToUtc :: Screening -> UTCTime
showtimeToUtc = posixSecondsToUTCTime . fromIntegral . showtime

dayOf :: Screening -> Day
dayOf = localDay . zonedTimeToLocalTime . utcToZonedTime tz . showtimeToUtc
  where
    tz = hoursToTimeZone (-5)

data Catalog = Catalog
  { venues :: [Venue]
  , films :: [Film]
  , screenings :: [Screening]
  }
  deriving Show

instance FromJSON Catalog where
  parseJSON (Object o) =
    Catalog
      <$> o .: "venues"
      <*> o .: "films"
      <*> o .: "screenings"
  parseJSON _ = error "invalid catalog json"

instance ToJSON Catalog where
  toJSON Catalog{..} =
    object
      [ "venues" .= venues
      , "films" .= films
      , "screenings" .= screenings
      ]

newtype ScreeningGroup = ScreeningGroup
  { unGroup :: [MarkedScreening]
  }
  deriving (Show,Eq)

type ScheduleState = M.Map FilmId ScreeningGroup

newtype PersistState = PersistState [(ScreeningId,ScreeningStatus,Pinned)]
  deriving Show

instance ToJSON PersistState where
  toJSON (PersistState ss) =
    object [ "ps" .= (toObject <$> ss)]
    where
      toObject (sid,stat,pinned) =
        object [ "i" .= sid, "s" .= stat, "p" .= pinned ]

instance FromJSON PersistState where
  parseJSON (Object o) = do
    let
      fromObject (Object o') =
        (,,) <$> o' .: "i" <*> o' .: "s" <*> o' .: "p"
      fromObject _ = error "invalid persist state json"
    Array ps <- o .: "ps"

    PersistState <$> mapM fromObject (V.toList ps)
  parseJSON _ = error "expected persist state json to be object"

toPersistState :: ScheduleState -> PersistState
toPersistState ss = PersistState $ concat ps
  where
    ps = fmap toPs . unGroup <$> M.elems ss
    toPs MarkedScreening{..} = (screeningId screening, status, pinned)

type ScreeningMap = M.Map ScreeningId Screening

fromPersistState :: ScreeningMap -> PersistState -> ScheduleState
fromPersistState smap (PersistState ps) =
  M.fromList $ toScreeningGroup <$> groups
  where
    groups =
      NE.groupAllWith (scFilmId . screening) (fromPs <$> ps)
    toScreeningGroup ms =
      (scFilmId . screening $ NE.head ms, ScreeningGroup (NE.toList ms))
    fromPs (sid, status, pinned) =
      MarkedScreening status pinned (fromJust (M.lookup sid smap))


data Command
  = Add Screening
  | RuleOut Screening
  | Pin Screening
  | UnPin Screening
  | RemoveFilm Screening
  | Clear
  deriving Show
