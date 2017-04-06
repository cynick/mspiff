module Mspiff.Vis
    where

import Control.Arrow ((&&&))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.List as DL
import qualified Data.List.NonEmpty as NE
import Data.Aeson
import Data.Maybe
import Data.Time

import Lucid
import Mspiff.Html
import Mspiff.Model
import Mspiff.Util

data VisItem = VisItem
  { itemId :: Int
  , startDate :: UTCTime
  , endDate :: UTCTime
  , itemContent :: T.Text
  , group :: VenueId
  }
  deriving Show

instance ToJSON VisItem where
  toJSON VisItem{..} =
    object
      [ "id" .= itemId
      , "start" .= startDate
      , "end" .= endDate
      , "content" .= itemContent
      , "group" .= group
      ]

data VisGroup = VisGroup
  { groupId :: VenueId
  , groupContent :: Name
  }
  deriving Show

instance ToJSON VisGroup where
  toJSON VisGroup{..} =
    object
      [ "id" .= groupId
      , "content" .= groupContent
      ]

data VisOptions = VisOptions
  { minTs :: UTCTime
  , maxTs :: UTCTime
  }
  deriving Show

instance ToJSON VisOptions where
  toJSON VisOptions{..} =
    object
      [ "zoomable" .= False
      , "moveable" .= False
      , "showCurrentTime" .= False
      , "min" .= minTs
      , "max" .= maxTs
      ]

data VisData = VisData
  { visItems :: [VisItem]
  , visGroups :: [VisGroup]
  , visOptions :: VisOptions
  }
  deriving Show

instance ToJSON VisData where
  toJSON VisData{..} =
    object
      [ "items" .= visItems
      , "groups" .= visGroups
      , "options" .= visOptions
      ]

buildVisData :: Catalog -> [VisData]
buildVisData Catalog{..} = toVisData <$> screeningGroups
  where
    screeningGroups = NE.toList <$> NE.groupAllWith dayOf screenings
    toVisData ss = VisData (concat items) groups (VisOptions minTs maxTs)
      where
        items = (NE.toList . fmap toItem) <$> NE.groupAllWith scVenueId ss
        toItem s = VisItem{..}
          where
            itemId = screeningId s
            startDate = showtimeToUtc s
            endDate = endDateFor s
            itemContent = LT.toStrict $ renderText (renderScreening s t)
            group = fromJust $ DL.lookup (scVenueId s) groupIdMap
            t = filmTitle (findFor films filmId (scFilmId s))
        endDateFor s =
          addUTCTime (fromIntegral (duration s)) (showtimeToUtc s)
        groupIdMap = zip vids [0..]
          where
            vids =
              venueId <$> DL.sort (DL.nub (findFor venues venueId . scVenueId <$> ss))
        groups = toGroup <$> groupIdMap
        toGroup (vid,groupId) = VisGroup groupId name
          where
            name = venueName $ findFor venues venueId vid
        dateRanges = (showtimeToUtc &&& endDateFor) <$> ss
        minTs = minimum $ fst <$> dateRanges
        maxTs = maximum $ snd <$> dateRanges
