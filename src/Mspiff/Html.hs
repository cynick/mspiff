module Mspiff.Html where

import qualified Data.Text as T
import qualified Data.List as DL
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty
import Data.Time
import Data.Monoid
import Data.Maybe

import Lucid
import Lucid.Base
import Lucid.Bootstrap

import Mspiff.Model

dayOf :: Screening -> Day
dayOf = utctDay . showtimeToUtc

page :: Catalog -> Html ()
page = doctypehtml_ . renderWholeSchedule

at :: T.Text -> T.Text -> Attribute
at = makeAttribute

toText :: Show s => s -> T.Text
toText = T.pack . show

script :: Monad m => T.Text -> HtmlT m ()
script s =
  with (makeElement "script") [ at "src" s
                              , at "type" "application/javascript"
                              ] ""

renderWholeSchedule :: Catalog -> Html ()
renderWholeSchedule (Catalog films ss) = body
  where
    body = div_ [id_ "container"] (dataContainer >> timelineContainer >> renderTimeline)
    days = NE.groupAllWith dayOf ss
    dataContainer =
      div_ [ id_ "schedule-data"
           , at "data-day-count" (toText (DL.length days))
           , style_ "visibility: hidden"
           ] renderDays
    timelineContainer =
      row_ [ id_ "timelines"
--           , class_ "zoomTarget"
           , at "data-targetsize" "0.50"
           , at "data-closeclick" "true"
           ] renderTimelines
    renderTimelines = mapM_ renderDayTimeline [1.. DL.length days]
    renderDayTimeline :: Int -> Html ()
    renderDayTimeline d =
      div_ [ id_ ("day-timeline-" <> toText d) ] ""
    renderDays = mapM_ (renderDayData films) (DL.zip [1..] days)
    renderTimeline = script_ "Mspiff.renderTimeline()"

renderDayData :: [Film] -> (Int, NonEmpty Screening) -> Html ()
renderDayData films (day, ss) = container renderVenues
  where
    container = div_ [ id_ ("day-data-" <> toText day)
                     , at "data-venue-count" (toText (DL.length venues))
                     ]
    renderVenues = mapM_ (renderVenue films) (DL.zip [1..] venues)
    venues = NE.groupAllWith venue (NE.toList ss)

renderVenue :: [Film] -> (Int, NonEmpty Screening) -> Html ()
renderVenue films (venue, ss) = row renderScreenings
  where
    row = div_ [ class_ ("venue-" <> toText venue)
               , at "data-venue-name" (toText venue)
               ]
    renderScreenings = mapM_ (renderScreening films) ss

titleOf :: [Film] -> Screening -> T.Text
titleOf films s = filmTitle film
  where
    film = fromJust (DL.find (\f -> filmId f == scFilmId s) films)

renderScreening :: [Film] -> Screening -> Html ()
renderScreening films s = do
  let
    startTime = showtimeToUtc s
    utcToText = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"
  div_ [ class_ "screening"
       , id_ ("screening-" <> toText (screeningId s))
       , at "data-start" (utcToText startTime)
       , at "data-end" (utcToText (addUTCTime (fromIntegral (duration s)) startTime))
       ] $ do
    div_ [class_ "film-title"] (toHtml (titleOf films s))
    control

icon :: T.Text -> Html ()
icon c = i_ [class_ ("fa icon-resize-small" <> " " <> c)] ""

control :: Html ()
control =
  div_ [class_ "control icon-bar"] $ do
    a_ [class_ "active", href_ "#"] $ icon "fa fa-plus"
    a_ [href_ "#"] $ icon "fa-minus"
    a_ [href_ "#"] $ icon "fa-circle-o"
    a_ [href_ "#"] $ icon "fa-times"
