module Mspiff.Html where

import qualified Data.Text as T
import qualified Data.List as DL
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty
import Data.Time
import Data.Monoid

import Lucid
import Lucid.Base
import Lucid.Bootstrap

import Mspiff.Model
import Mspiff.Loader

dayOf :: Screening -> Day
dayOf = utctDay . showtimeToUtc

page :: WholeSchedule -> Html ()
page = doctypehtml_ . renderWholeSchedule

at :: T.Text -> T.Text -> Attribute
at = makeAttribute

toText :: Show s => s -> T.Text
toText = T.pack . show

script :: Monad m => T.Text -> HtmlT m ()
script s =
  with (makeElement "script") [at "src" s
                              , at "type" "application/javascript"
                              ] ""

renderWholeSchedule :: WholeSchedule -> Html ()
renderWholeSchedule (Schedule ss) = header >> body_ body
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
    renderDays = mapM_ renderDayData (DL.zip [1..] days)
    renderTimeline = script_ "Mspiff.renderTimeline()"

header :: Html ()
header = head_ $ do
  meta_ [at "charset" "utf-8"]
  meta_ [at "http-equiv" "X-UA-Compatible", at "content" "IE=edge"]
  meta_ [at "name""viewport", at "content" "width=device-width, initial-scale=1"]

  title_ "MSIPFF Navigator"

  script "http://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js"
  script "http://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/jquery-ui.min.js"

  script "http://local.hoffmanavenuesoftware.com/js/bootstrap.min.js"
  script "http://local.hoffmanavenuesoftware.com/js/zoomooz.min.js"
  script "https://cdnjs.cloudflare.com/ajax/libs/jquery-easing/1.3/jquery.easing.min.js"

  link_ [ href_ "http://local.hoffmanavenuesoftware.com/css/bootstrap.min.css"
        , rel_ "stylesheet"
        ]
  link_ [ href_ "http://local.hoffmanavenuesoftware.com/css/mspiff.css"
        , rel_ "stylesheet"
        ]
  link_ [ href_ "http://local.hoffmanavenuesoftware.com/css/vis-timeline-graph2d.min.css"
        , rel_ "stylesheet"
        ,  type_ "text/css"
        ]
  script "https://use.fontawesome.com/86661df918.js"
  script "http://local.hoffmanavenuesoftware.com/js/vis.js"
  script "http://local.hoffmanavenuesoftware.com/js/mspiff.js"

renderDayData :: (Int, NonEmpty Screening) -> Html ()
renderDayData (day, ss) = container renderVenues
  where
    container = div_ [ id_ ("day-data-" <> toText day)
                     , at "data-venue-count" (toText (DL.length venues))
                     ]
    renderVenues = mapM_ renderVenue (DL.zip [1..] venues)
    venues = NE.groupAllWith venue (NE.toList ss)

renderVenue :: (Int, NonEmpty Screening) -> Html ()
renderVenue (venue, ss) = row renderScreenings
  where
    row = div_ [ class_ ("venue-" <> toText venue)
               , at "data-venue-name" (toText venue)
               ]
    renderScreenings = mapM_ renderScreening ss

titleOf :: Screening -> T.Text
titleOf = filmTitle . filmOf

renderScreening :: Screening -> Html ()
renderScreening s = do
  let
    startTime = showtimeToUtc s
    utcToText = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"
  div_ [ class_ "screening"
       , id_ ("screening-" <> toText (screeningId s))
       , at "data-start" (utcToText startTime)
       , at "data-end" (utcToText (addUTCTime (fromIntegral (duration s)) startTime))
       ] $ do
    div_ [class_ "film-title"] (toHtml (titleOf s))
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
