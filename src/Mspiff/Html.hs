module Mspiff.Html where

import qualified Data.Text as T
import qualified Data.List as DL
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty
import Data.Time
import Data.Monoid
import Control.Monad

import Lucid
import Lucid.Base

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
script s = with (makeElement "script") [at "src" s
                                        , at "type" "application/javascript"] ""

renderWholeSchedule :: WholeSchedule -> Html ()
renderWholeSchedule (Schedule ss) = header >> container renderDays
  where
    days = NE.groupAllWith dayOf ss
    container =  div_ [ id_ "schedule-data"
                      , at "data-day-count" (toText (DL.length days))
                      , style_ "visibility: hidden"
                      ]
    renderDays = mapM_ renderDay (DL.zip [1..] days)

header :: Html ()
header = head_ $ do

  meta_ [at "http-equiv" "X-UA-Compatible", at "content" "IE=edge"]
  meta_ [at "name""viewport", at "content" "width=device-width, initial-scale=1"]

  title_ "MSIPFF Navigator"

  link_ [ href_ "http://local.hoffmanavenuesoftware.com/css/bootstrap.min.css"
        , rel_ "stylesheet"
        ]

  script "http://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js"
  script "http://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/jquery-ui.min.js"
  script "http://local.hoffmanavenuesoftware.com/js/bootstrap.min.js"
  script "http://local.hoffmanavenuesoftware.com/js/mspiff.js"
  script "https://cdnjs.cloudflare.com/ajax/libs/vis/4.19.1/vis.min.js"
  script "https://cdnjs.cloudflare.com/ajax/libs/jquery-easing/1.3/jquery.easing.min.js"

  link_ [ href_ "http://local.hoffmanavenuesoftware.com/css/font-awesome.min.css"
        , rel_ "stylesheet"
        , type_ "text/css"
        ]
  link_ [ href_ "https://fonts.googleapis.com/css?family=Lora:400,700,400italic,700italic"
        , rel_ "stylesheet"
        , type_ "text/css"
        ]
  link_ [ href_ "https://fonts.googleapis.com/css?family=Montserrat:400,700"
        , rel_ "stylesheet"
        , type_ "text/css"]


renderDay :: (Int, NonEmpty Screening) -> Html ()
renderDay (day, ss) = container renderVenues
  where
    container = div_ [ id_ ("day-" <> toText day)
                     , class_ "day"
                     , at "data-venue-count" (toText (DL.length venues))
                     ]
    renderVenues = mapM_ renderVenue (DL.zip [1..] venues)
    venues = NE.groupAllWith screen (NE.toList ss)

renderVenue :: (Int, NonEmpty Screening) -> Html ()
renderVenue (venue, ss) = row renderScreenings
  where
    row = div_ [class_ ("venue-" <> toText venue)]
    renderScreenings = mapM_ renderScreening ss

titleOf :: Screening -> T.Text
titleOf = filmTitle . filmOf

renderScreening :: Screening -> Html ()
renderScreening s = do
  let ts = showtimeToUtc s
  div_ [ class_ "screening"
       , id_ ("screening-" <> toText (screeningId s))
       , at "data-start" (toText ts)
       , at "data-end" (toText (addUTCTime (fromIntegral (duration s)) ts))
       ] $ do
    div_ [class_ "film-title"] (toHtml (titleOf s))
    control

icon :: T.Text -> Html ()
icon c = i_ [class_ c] ""

control :: Html ()
control =
  div_ [class_ "control"] $ do
    icon "fa-plus"
    icon "fa-minus"
    icon "fa-circle-o"
    icon "fa-times"
