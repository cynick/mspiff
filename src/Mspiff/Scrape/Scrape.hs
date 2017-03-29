
module Mspiff.Scrape.Scrape
    where

import Mspiff.Model
import qualified Data.Text.Lazy as T
import qualified Data.List as DL
import Data.Aeson
--import Data.ByteString.Lazy
import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.Text.Lazy.Encoding
import qualified Network.URI.Encode as E
import Network.HTTP.Client
import Text.HTML.TagSoup


import Data.Time
import Data.Time.Clock.POSIX

type Name = T.Text
data ScrapeScreening = ScrapeScreening
  { screeningName :: Name
  , screeningStartTime :: Maybe UTCTime
  , screeningDuration :: Int
  , screeningVenue :: Name
  , screeningUrl :: T.Text
  }
  deriving (Show, Eq)

parseDate :: T.Text -> Maybe UTCTime
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" . T.unpack

instance FromJSON ScrapeScreening where
  parseJSON (Object v) =
    ScrapeScreening
      <$> v .: "name"
      <*> (parseDate <$> v .: "startDate")
      <*> pure 0
      <*> (v .: "location" >>= (.: "name"))
      <*> (v .: "offers" >>= (.: "url"))
  parseJSON _ = error "invalid screening json"


scheduleUrlBase :: T.Text
scheduleUrlBase = "http://prod3.agileticketing.net/WebSales/pages/list.aspx?epguid=87ecbce5-5fcd-406d-8bcb-88fa6bb54965&mdy="

toDateUrl :: Day -> (Day, T.Text)
toDateUrl d = (d, scheduleUrlBase <> T.pack (E.encode str) <> "&")
  where
    str = formatTime defaultTimeLocale "%-m/%d/%y" (UTCTime d 0)

processAll :: IO [ScrapeScreening]
processAll = join <$> mapM processUrl (toDateUrl <$> days)
  where days = DL.take 16 [(fromGregorian 2017 4 13) ..]

processUrl :: (Day, T.Text) -> IO [ScrapeScreening]
processUrl (d, url) = processTags d . parseTags <$> hit url

processLd :: [Tag T.Text] -> Maybe T.Text
processLd = go
  where
    go [] = Nothing
    go (x:y:xs) | isLdJson x && isTagText y = Just (fromTagText y)
                | otherwise = go xs
    isLdJson t =
      isTagOpenName "script" t && fromAttrib "type" t == "application/ld+json"

processTags :: Day -> [Tag T.Text] -> [ScrapeScreening]
processTags day tags = foldr combine [] (fromMaybe [] screenings)
  where
    screenings = processLd tags >>= decode' . encodeUtf8

    durations = go [] tags
      where
        go :: [(Name,Int)] -> [Tag T.Text] -> [(Name,Int)]
        go acc [] = acc
        go acc list@(se:item:_:timeVal:close:name:nameVal:xs)
          | isSE se && isName name && isItem item && isTagClose close =
             go ( (fromTagText nameVal, parseDuration se) : acc) xs
          | otherwise = go acc (DL.drop 1 list)
        go acc _ = acc
        parseDuration = read . T.unpack . fromAttrib "duration"
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
  return $ decodeUtf8 (responseBody response)
