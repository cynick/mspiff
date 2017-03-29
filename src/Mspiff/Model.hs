module Mspiff.Model where

import Prelude

import Data.Aeson hiding (Array)
import qualified Data.Text as T

import Data.Time
import Data.Time.Clock.POSIX
import Data.Monoid

type FilmId = Int
type ScreeningId = Int
type Title = T.Text
type Showtime = Int
type Duration = Int
type Venue = T.Text
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

instance FromJSON Film where
  parseJSON (Object v) =
    Film
      <$> v .: "filmId"
      <*> v .: "filmTitle"
      <*> pure []
      <*> v .: "filmUrl"

  parseJSON _ = error "invalid film json"

instance ToJSON Film where
  toJSON Film{..} =
    object
      [ "filmId" .= filmId
      , "filmTitle" .= filmTitle
      , "filmScreenings" .= filmScreenings
      , "filmUrl" .= filmUrl
      ]

data Screening = Screening
  { scFilmId :: FilmId
  , screeningId :: ScreeningId
  , overlapping :: [Screening]
  , otherScreening :: Maybe Screening
  , showtime :: Showtime
  , duration :: Duration
  , venue :: Venue
  }

instance ToJSON Screening where
  toJSON Screening{..} =
    object
      [ "scFilmId" .= scFilmId
      , "screeningId" .= screeningId
      , "overlapping" .= overlapping
      , "otherScreening" .= otherScreening
      , "showtime" .= showtime
      , "duration" .= duration
      , "venue" .= venue
      ]

instance Show Screening where
  show s =
    "Screening {scFilmId = " <> show (scFilmId s) <>
    ", screeningId = " <> show (screeningId s) <>
    ", overlapping = " <> show (screeningId <$> overlapping s) <>
    ", otherScreening = " <> show (screeningId <$> otherScreening s) <>
    ", showtime = " <> show (showtime s) <>
    ", duration = " <> show (duration s) <>
    ", venue = " <> show (venue s) <>
    "}"

instance Eq Screening where
  a == b = screeningId a == screeningId b

instance Ord Screening where
  compare a b =
    case showtime a `compare` showtime b of
      LT -> LT
      GT -> GT
      EQ -> venue a `compare` venue b

instance FromJSON Screening where
  parseJSON (Object v) =
    Screening
     <$> v .: "scFilmId"
     <*> v .: "screeningId"
     <*> pure []
     <*> pure Nothing
     <*> v .: "showtime"  -- seconds since Epoch
     <*> v .: "duration" -- duration is given in minutes
     <*> v .: "venue"

  parseJSON _ = error "invalid screening json"

data Pinned = Pinned | Unpinned deriving (Show,Eq)
data ScreeningStatus =
  Unscheduled | Scheduled | Impossible | RuledOut | OtherScheduled
  deriving (Enum, Show, Eq)

data MarkedScreening = MarkedScreening
  { status :: ScreeningStatus
  , pinned :: Pinned
  , screening :: Screening
  }
  deriving Show

instance Eq MarkedScreening where
  a == b = screening a == screening b

isRuledOut :: MarkedScreening -> Bool
isRuledOut = (==RuledOut) . status

isPinned :: MarkedScreening -> Bool
isPinned = (==Pinned) . pinned

isOther :: Screening -> Screening -> Bool
isOther s s' = s /= s' && scFilmId s' == scFilmId s

newtype Schedule = Schedule { scheduleScreenings :: [Screening] }
  deriving (Eq,Show,Monoid)

type WholeSchedule = Schedule
type DaySchedule = Schedule
type VenueSchedule = Schedule
type ViewableSchedule = Schedule
type Catalog = [Film]

showtimeToUtc :: Screening -> UTCTime
showtimeToUtc = posixSecondsToUTCTime . fromIntegral . showtime



