{-# LANGUAGE JavaScriptFFI, CPP #-}
module Main where

import Prelude hiding (log)
import Control.Monad
import Control.Concurrent
import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal(fromJSVal)
import GHCJS.Foreign.Callback (Callback, syncCallback1, OnBlocked(ContinueAsync))
import GHCJS.Prim
import Data.JSString.Text

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.List as DL
import Data.FileEmbed
import Data.Default
import Data.Monoid

import JavaScript.JQuery
import JavaScript.JQuery.Internal

import Lucid

import Mspiff.Model
import Mspiff.Loader
import Mspiff.Scheduler
import Mspiff.Html

catalogJson :: BS.ByteString
catalogJson = $(embedFile "data/catalog")

foreign import javascript unsafe "Mspiff.renderDayTimeline($1)"
 renderDayTimeline :: Int -> IO ()

foreign import javascript unsafe "alert($1)"
 alert :: JSString -> IO ()

foreign import javascript unsafe "console.log($1)"
 log_ :: JSVal -> IO ()

turnOffSpinner :: IO ()
turnOffSpinner = do
  node <- select "#loading"
  void $ setCss "visibility" "hidden" node

log :: String -> IO ()
log x = log_ $ toJSString ("HS: " <> x)

s2js :: String -> JSString
s2js = textToJSString . T.pack

idFor :: Screening -> JSString
idFor s = s2js $ "#screening-" <> show (screeningId s)

setupHandlers :: [Screening] -> IO ()
setupHandlers ss = mapM_ selectorFor ss
  where
    selectorFor s = select (idFor s) >>= click (handler s) def
    setColor c s =
      select (idFor s) >>= parent >>= parent >>= setCss "background-color" c
    handler s _ = do
      setColor "green" s
      mapM_ (setColor "orange") (others s)
      mapM_ (setColor "red") (overlapping s)

main = do
  let
    Just catalog = loadCatalog (LBS.fromStrict catalogJson)
    html = renderText (renderWholeSchedule catalog)
    html' = textToJSString (LT.toStrict html)
    sched = M.empty
  select "body" >>= append html'
  let
    ss = screenings catalog
    days = DL.length $ DL.nub $ dayOf <$> ss
  mapM_ renderDayTimeline [1..days]
  turnOffSpinner
  setupHandlers ss


