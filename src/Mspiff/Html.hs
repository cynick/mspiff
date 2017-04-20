module Mspiff.Html where

import qualified Data.Text as T
import qualified Data.List as DL
import Data.Monoid
import Data.Maybe

import Lucid
import Lucid.Base

import Mspiff.Model

at :: T.Text -> T.Text -> Attribute
at = makeAttribute

toText :: Show s => s -> T.Text
toText = T.pack . show

titleOf :: [Film] -> Screening -> T.Text
titleOf films s = filmTitle film
  where
    film = fromJust (DL.find (\f -> filmId f == scFilmId s) films)

renderScreening :: Screening -> T.Text -> Html ()
renderScreening s name =
  div_ [ class_ "screening"
       , id_ ("screening-" <> toText (screeningId s))
       ] $ do
    div_ [class_ "film-title"] (toHtml name)
    div_ [class_ "control remove-film"] (a_ [href_ "#"] $ icon "fa-times")
    div_ [] $ do
      screeningStatus
      pin
      remove

icon :: T.Text -> Html ()
icon c = i_ [class_ ("fa icon-resize-small" <> " " <> c)] ""

remove :: Html ()
remove = div_ attrs (a_ [href_ "#"] $ icon "fa-minus")
  where
    attrs =
      [ class_ "control ruleout-screening"
      , at "title" "Rule Out Screening"
      ]

pin :: Html ()
pin = div_ attrs (a_ [href_ "#"] $ icon "fa-circle-o")
  where
    attrs =
      [ class_ "control pin-screening unpinned"
      , at "title" "Pin Screening"
      ]

screeningStatus :: Html ()
screeningStatus = div_ attrs (return ())
  where
    attrs =
      [ class_ "screening-status control"
      , at "title" ""
      ]

info :: Html ()
info = div_ [class_ "screening-info"] (a_ [href_ "#"] $ icon "fa-info")

