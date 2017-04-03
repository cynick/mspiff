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
