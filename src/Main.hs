{-# LANGUAGE JavaScriptFFI, CPP #-}
module Main where

import Control.Monad
import Control.Concurrent
import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal(fromJSVal)
import GHCJS.Foreign.Callback (Callback, syncCallback1, OnBlocked(ContinueAsync))
import GHCJS.Prim
import Data.JSString.Text

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS

import Data.FileEmbed

import JavaScript.JQuery
import JavaScript.JQuery.Internal

import Lucid

import Mspiff.Model
import Mspiff.Loader
import Mspiff.Scheduler
import Mspiff.Html

catalogJson :: BS.ByteString
catalogJson = $(embedFile "data/catalog")

main = do
  let
    Just catalog = loadCatalog (LBS.fromStrict catalogJson)
    html = renderText (renderWholeSchedule catalog)
    html' = textToJSString (T.toStrict html)
  b <- select "body"
  replaceWith html' b

{-
handler :: JQuery -> Event -> IO ()
handler m e = do
  x <- pageX e
  y <- pageY e
  setText (T.pack $ show (x,y)) m
  return ()

-}
