
module Mspiff.Scrape.Scrape
    where

import Mspiff.Model
import qualified Data.Text.Lazy as T
import qualified Data.List as DL
import Data.ByteString.Lazy
import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.Text.Lazy.Encoding
import Network.URI.Encode
import Network.HTTP.Client
import Text.HTML.TagSoup

import Data.Time
import Data.Time.Clock.POSIX

data ScrapeScreening = ScrapeScreening
  { screeningName :: T.Text
  , screeningStartTime :: Int
  , screeningDuration :: Int
  }
  deriving (Show, Eq)

scheduleUrlBase :: T.Text
scheduleUrlBase = "http://prod3.agileticketing.net/WebSales/pages/list.aspx?epguid=87ecbce5-5fcd-406d-8bcb-88fa6bb54965&mdy="

toDateUrl :: Day -> (Day, T.Text)
toDateUrl d = (d, scheduleUrlBase <> T.pack (encode str) <> "&")
  where
    str = formatTime defaultTimeLocale "%-m/%d/%y" (UTCTime d 0)

processAll :: IO [ScrapeScreening]
processAll = join <$> mapM processUrl (toDateUrl <$> days)
  where days = DL.take 16 [(fromGregorian 2017 4 13) ..]

processUrl :: (Day, T.Text) -> IO [ScrapeScreening]
processUrl (d, url) = processTags d . parseTags <$> hit url

parseTOD :: Day -> T.Text -> Maybe Showtime
parseTOD day t = (fromIntegral . floorInt . utcTimeToPOSIXSeconds) <$> toUtc
   where
     floorInt :: RealFrac a => a -> Int
     floorInt = floor
     t' = T.unpack (T.takeWhile (/='(') t)
     toUtc = UTCTime day . utctDayTime <$> toTod
     toTod = parseTimeM True defaultTimeLocale "%l:%M %p " t'

processTags' :: [Tag T.Text] -> Maybe T.Text
processTags' = go
  where
    go [] = Nothing
    go (x:y:xs) | isLdJson x && isTagText y = Just (fromTagText y)
                | otherwise = go xs
    isLdJson t =
      isTagOpenName "script" t && fromAttrib "type" t == "application/ld+json"

processTags :: Day -> [Tag T.Text] -> [ScrapeScreening]
processTags day = DL.reverse . go []
  where
    go :: [ScrapeScreening] -> [Tag T.Text] -> [ScrapeScreening]
    go acc [] = acc
    go acc list@(se:item:_:timeVal:close:name:nameVal:xs)
      | isSE se && isName name && isItem item && isTagClose close =
         go (toScreening se timeVal nameVal : acc) xs
      | otherwise = go acc (DL.drop 1 list)
    go acc _ = acc

    isDiv = isTagOpenName "div"

    hasClass c t = isDiv t && fromAttrib "class" t == c
    isSE = hasClass "ScheduledEvent"
    isItem = hasClass "Item"
    isName = hasClass "Name"
    fromTime = fromMaybe 0 . parseTOD day

    toScreening dur t name =
      ScrapeScreening
        (fromTagText name)
        (fromTime (fromTagText t))
        (read $ T.unpack (fromAttrib "duration" dur))

hit :: T.Text -> IO T.Text
hit url = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest (T.unpack url)
  response <- httpLbs request manager
  return $ decodeUtf8 (responseBody response)
