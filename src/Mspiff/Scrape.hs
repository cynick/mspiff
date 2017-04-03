
module Mspiff.Scrape
    where

import Control.Arrow ((&&&))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.List as DL
import qualified Data.List.NonEmpty as NE
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Control.Monad
import Data.Monoid
import Data.Function (on)
import Data.Maybe
import Data.Text.Lazy.Encoding
import qualified Network.URI.Encode as E
import Network.HTTP.Client
import Text.HTML.TagSoup

import Data.Time
import Data.Time.Clock.POSIX

import Lucid
import Mspiff.Html
import Mspiff.Model

type Name = T.Text
data ScrapeScreening = ScrapeScreening
  { screeningName :: Name
  , screeningStartTime :: UTCTime
  , screeningDuration :: Int
  , screeningVenue :: Name
  , screeningUrl :: T.Text
  }
  deriving (Show, Read, Eq)

instance FromJSON ScrapeScreening where
  parseJSON (Object v) =
    ScrapeScreening
      <$> v .: "name"
      <*> (fromJust . parseDate <$> v .: "startDate")
      <*> pure 0
      <*> (v .: "location" >>= (.: "name"))
      <*> (v .: "offers" >>= (.: "url"))
  parseJSON _ = error "invalid screening json"

data VisItem = VisItem
  { itemId :: Int
  , startDate :: UTCTime
  , endDate :: UTCTime
  , itemContent :: T.Text
  , group :: VenueId
  , title :: Name
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
      , "title" .= title
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
  { visItems :: [[VisItem]]
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

parseDate :: T.Text -> Maybe UTCTime
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" . T.unpack

fromUtc :: UTCTime -> Int
fromUtc = fromIntegral . floorInt . utcTimeToPOSIXSeconds
  where
    floorInt :: RealFrac a => a -> Int
    floorInt = floor

findFor :: Eq b => [a] -> (a -> b) -> b -> a
findFor xs f name = fromJust (DL.find filt xs)
  where filt x = f x == name

scheduleUrlBase :: T.Text
scheduleUrlBase = "http://prod3.agileticketing.net/WebSales/pages/list.aspx?epguid=87ecbce5-5fcd-406d-8bcb-88fa6bb54965&mdy="

toDateUrl :: Day -> T.Text
toDateUrl d = scheduleUrlBase <> T.pack (E.encode str) <> "&"
  where
    str = formatTime defaultTimeLocale "%-m/%d/%y" (UTCTime d 0)

-- This function creates the Vis data for a single day timeline
toVisData :: [Venue] -> [Film] -> [Screening] -> VisData
toVisData vs fs ss = VisData items groups (VisOptions minTs maxTs)
  where
    items = fmap (NE.toList . fmap toItem) $ NE.groupAllWith scVenueId ss
    toItem s = VisItem{..}
      where
        itemId = screeningId s
        startDate = showtimeToUtc s
        endDate = endDateFor s
        itemContent = LT.toStrict $ renderText (renderScreening s title)
        group = scVenueId s
        title = filmTitle (findFor fs filmId (scFilmId s))
    endDateFor s =
      addUTCTime (fromIntegral (duration s)) (showtimeToUtc s)
    venueIds = DL.sort (DL.nub (scVenueId <$> ss))
    groups = toGroup <$> venueIds
    toGroup vid = VisGroup vid name
      where
        name = venueName $ findFor vs venueId vid
    dateRanges = (showtimeToUtc &&& endDateFor) <$> ss
    minTs = minimum $ fst <$> dateRanges
    maxTs = maximum $ snd <$> dateRanges

writeCatalog :: [ScrapeScreening] -> IO ()
writeCatalog = BS.writeFile "data/catalog" . encode . buildCatalog

buildCatalog :: [ScrapeScreening] -> Catalog
buildCatalog scrapes = Catalog venues films screenings visData
  where
    toName ScrapeScreening{..} = (screeningName, screeningUrl)
    toFilm (idx,(screeningName,screeningUrl)) =
      Film idx screeningName [] screeningUrl
    filmNames =
      DL.sortBy (compare `on` fst) $ DL.nubBy ((==) `on` fst) $ toName <$> scrapes
    films = DL.sort $ toFilm <$> zip [0..] filmNames
    venueNames = DL.sort $ DL.nub $ screeningVenue <$> scrapes
    venues = uncurry Venue <$> zip [0..] venueNames
    toScreening (idx, ScrapeScreening{..}) acc =
       Screening
         (filmId (findFor films filmTitle screeningName))
         idx
         []
         []
         (fromUtc screeningStartTime)
         screeningDuration
         (venueId (findFor venues venueName screeningVenue))
         : acc
    screenings =
      DL.sort $ DL.reverse $ foldr toScreening [] (zip [0..] scrapes)
    visData = (encodeVisData . toVisData venues films) <$> groups
      where
        groups = NE.toList <$> NE.groupAllWith dayOf screenings
        encodeVisData = LT.toStrict . decodeUtf8 . encode

scrapeAll :: IO [ScrapeScreening]
scrapeAll = join <$> mapM processUrl (zip days (toDateUrl <$> days))
  where days = DL.take 17 [(fromGregorian 2017 4 13) ..]

processUrl :: (Day, T.Text) -> IO [ScrapeScreening]
processUrl (day, url) = do
  c <- hit url
  let r = processTags . parseTags $ c
  putStrLn $ "---- " ++ show day ++ " ----"
  mapM_ print (screeningName <$> r)
  return r

processLd :: [Tag T.Text] -> Maybe T.Text
processLd = go
  where
    go (x:y:xs) | isLdJson x && isTagText y = Just (fromTagText y)
                | otherwise = go xs
    go _ = Nothing
    isLdJson t =
      isTagOpenName "script" t && fromAttrib "type" t == "application/ld+json"

processTags :: [Tag T.Text] -> [ScrapeScreening]
processTags tags = foldr combine [] (fromMaybe [] screenings)
  where
    screenings = processLd tags >>= decode' . encodeUtf8 . LT.fromStrict
    durations = go [] tags
      where
        go :: [(Name,Int)] -> [Tag T.Text] -> [(Name,Int)]
        go acc [] = acc
        go acc list@(se:item:_:_:close:name:nameVal:xs)
          | isSE se && isName name && isItem item && isTagClose close =
             go ( (fromTagText nameVal, parseDuration se) : acc) xs
          | otherwise = go acc (DL.drop 1 list)
        go acc _ = acc
        parseDuration = (*60) . read . T.unpack . fromAttrib "duration"
        isDiv = isTagOpenName "div"
        hasClass c t = isDiv t && fromAttrib "class" t == c
        isSE = hasClass "ScheduledEvent"
        isItem = hasClass "Item"
        isName = hasClass "Name"

    combine s@ScrapeScreening{..} acc =
      let d = DL.lookup screeningName durations
      in maybe (s:acc) (\d' -> s { screeningDuration = d' } : acc) d

hit :: T.Text -> IO T.Text
hit url = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest (T.unpack url)
  response <- httpLbs request manager
  return $ LT.toStrict (decodeUtf8 (responseBody response))
