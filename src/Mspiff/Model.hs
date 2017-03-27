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
type Screen = T.Text

data Film = Film
  { filmId :: FilmId
  , filmTitle :: Title
  , filmScreenings :: [Screening]
   --  ... other attributes as they become interesting.
  }
  deriving (Show,Ord)

instance Eq Film where
  a == b = filmId a == filmId b

instance FromJSON Film where
  parseJSON (Object v) =
    Film <$>
      v .: "filmId" <*>
      v .: "filmTitle" <*>
      pure []

  parseJSON _ = error "invalid film json"

data Screening = Screening
  { scFilmId :: FilmId
  , screeningId :: ScreeningId
  , overlapping :: [Screening]
  , otherScreening :: Maybe Screening
  , showtime :: Showtime
  , duration :: Duration
  , screen :: Screen
  }

instance Show Screening where
  show s = "Screening {scFilmId = " <> show (scFilmId s) <>
           ", screeningId = " <> show (screeningId s) <>
           ", overlapping = " <> show (screeningId <$> overlapping s) <>
           ", otherScreening = " <> show (screeningId <$> otherScreening s) <>
           ", showtime = " <> show (showtime s) <>
           ", duration = " <> show (duration s) <>
           ", screen = " <> show (screen s) <>
           "}"

instance Eq Screening where
  a == b = screeningId a == screeningId b

instance Ord Screening where
  compare a b =
    case showtime a `compare` showtime b of
      LT -> LT
      GT -> GT
      EQ -> screen a `compare` screen b

instance FromJSON Screening where
  parseJSON (Object v) =
    Screening <$>
      v .: "scFilmId" <*>
      v .: "screeningId" <*>
      pure [] <*>
      pure Nothing <*>
      v .: "screeningTime" <*> -- seconds since Epoch
      ((60*) <$> v .: "duration" ) <*> -- duration is given in minutes
      v .: "screen"

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



