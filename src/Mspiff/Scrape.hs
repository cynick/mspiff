
module Mspiff.Scrape
    where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.List as DL
import qualified Data.Map.Strict as M
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

import Mspiff.Model
import Mspiff.Util

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


parseDate :: T.Text -> Maybe UTCTime
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" . T.unpack

fromUtc :: UTCTime -> Int
fromUtc = fromIntegral . floorInt . utcTimeToPOSIXSeconds
  where
    floorInt :: RealFrac a => a -> Int
    floorInt = floor

scheduleUrlBase :: T.Text
scheduleUrlBase = "http://prod3.agileticketing.net/WebSales/pages/list.aspx?epguid=87ecbce5-5fcd-406d-8bcb-88fa6bb54965&mdy="

toDateUrl :: Day -> T.Text
toDateUrl d = scheduleUrlBase <> T.pack (E.encode str) <> "&"
  where
    str = formatTime defaultTimeLocale "%-m/%d/%y" (UTCTime d 0)

writeCatalog :: [ScrapeScreening] -> IO ()
writeCatalog = BS.writeFile "data/catalog" . encode . buildCatalog

buildCatalog :: [ScrapeScreening] -> Catalog
buildCatalog scrapes = Catalog{..}
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
    filmMap = M.empty
    screeningMap = M.empty

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
