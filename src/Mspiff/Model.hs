module Mspiff.Model where

import Prelude
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Aeson
import Data.Monoid

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

type ScreeningMap = M.Map ScreeningId Screening

data Pinned = Pinned | Unpinned
  deriving (Show, Read, Eq, Bounded, Enum, Ord)

instance ToJSON Pinned where
  toJSON Pinned = Number 0
  toJSON Unpinned = Number 1

instance FromJSON Pinned where
  parseJSON (Number n) = return $ if n == 0 then Pinned else Unpinned
  parseJSON _ = error "invalid pinned json"

data ScreeningStatus =
  Unscheduled | Scheduled | Impossible | RuledOut | OtherPinned | OtherScheduled
  deriving (Eq, Bounded, Enum, Ord)

instance Show ScreeningStatus where
  show Unscheduled = "u"
  show Scheduled = "s"
  show Impossible = "i"
  show RuledOut = "r"
  show OtherPinned = "n"
  show OtherScheduled = "o"


instance ToJSON ScreeningStatus where
  toJSON Unscheduled = Number 0
  toJSON Scheduled = Number 1
  toJSON Impossible = Number 2
  toJSON RuledOut = Number 3
  toJSON OtherPinned = Number 4
  toJSON OtherScheduled = Number 5

instance FromJSON ScreeningStatus where
  parseJSON (Number n) =
    return $ case n of
      0 -> Unscheduled
      1 -> Scheduled
      2 -> Impossible
      3 -> RuledOut
      4 -> OtherPinned
      5 -> OtherScheduled
      _ -> error "unrecognized screening status"
  parseJSON _ = error "invalid screening status json"

data MarkedScreening = MarkedScreening
  { status :: ScreeningStatus
  , pinned :: Pinned
  , screening :: Screening
  }

instance Show MarkedScreening where
  show MarkedScreening{..} =
    concat [ show (screeningId screening)
           , "("
           , show status
           , if pinned == Pinned then "p" else ""
           , ")"
           ]

instance Eq MarkedScreening where
  a == b = screening a == screening b

instance Ord MarkedScreening where
  a `compare` b = screening a `compare` screening b

newtype Schedule = Schedule { scheduleScreenings :: [Screening] }
  deriving (Eq,Show,Monoid)

type WholeSchedule = Schedule
type VenueSchedule = Schedule
type ViewableSchedule = Schedule

data Catalog = Catalog
  { venues :: [Venue]
  , films :: [Film]
  , screenings :: [Screening]
  , filmMap :: M.Map FilmId Film
  , screeningMap :: M.Map ScreeningId Screening
  }
  deriving Show

instance FromJSON Catalog where
  parseJSON (Object o) =
    Catalog
      <$> o .: "venues"
      <*> o .: "films"
      <*> o .: "screenings"
      <*> pure M.empty
      <*> pure M.empty

  parseJSON _ = error "invalid catalog json"

instance ToJSON Catalog where
  toJSON Catalog{..} =
    object
      [ "venues" .= venues
      , "films" .= films
      , "screenings" .= screenings
      ]

type ScheduleState = M.Map FilmId [MarkedScreening]

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

data Command
  = Add Screening
  | RuleOut Screening
  | Pin Screening
  | UnPin Screening
  | RemoveFilm Screening
  | ShowBlurb Screening
  | Clear
  | Redraw
  deriving Show

