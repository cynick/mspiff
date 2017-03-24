
module Mspiff.Model where

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

data ScreeningStatus =
  Unscheduled | Scheduled | Impossible | RuledOut | OtherScheduled
  deriving (Enum, Show, Eq)

data Screening = Screening
  { scFilmId :: FilmId
  , screeningId :: ScreeningId
  , overlapping :: [Screening]
  , otherScreening :: Maybe Screening
  , showtime :: Showtime
  , duration :: Duration
  , screen :: Screen
  }
  deriving (Show)

instance Eq Screening where
  a == b = screeningId a == screeningId b

instance Ord Screening where
  compare a b =
    case showtime a `compare` showtime b of
      LT -> LT
      GT -> GT
      EQ -> screen a `compare` screen b

data Pinned = Pinned | Unpinned deriving (Show,Eq)
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

newtype Schedule = Schedule { scheduleScreenings :: [Screening] }
  deriving (Eq,Show,Monoid)

type WholeSchedule = Schedule
type DaySchedule = Schedule
type ViewableSchedule = Schedule
type Catalog = [Film]

showtimeToUtc :: Screening -> UTCTime
showtimeToUtc = posixSecondsToUTCTime . fromIntegral . showtime

dayOf :: Screening -> Day
dayOf = utctDay . showtimeToUtc

