{-# LANGUAGE JavaScriptFFI, CPP #-}
module Main where

import Prelude hiding (log)
import Control.Monad
import Control.Arrow
import Control.Concurrent
import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal(fromJSVal)
import GHCJS.Foreign.Callback
import GHCJS.Prim

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.List as DL
import Data.Aeson
import Data.Maybe
import Data.Text.Lazy.Encoding
import Data.FileEmbed
import Data.Default
import Data.Monoid
import Data.JSString
import JavaScript.JQuery
import JavaScript.JQuery.Internal

import Lucid

import Mspiff.Model
import Mspiff.Loader
import Mspiff.Scheduler
import Mspiff.Html
import Mspiff.Vis

catalogJson :: BS.ByteString
catalogJson = $(embedFile "data/catalog")

foreign import javascript unsafe "Mspiff.renderDayTimeline($1,$2)"
 renderDayTimeline :: Int -> JSString -> IO ()

foreign import javascript unsafe "Mspiff.setEventHandler()"
 setEventHandler :: IO ()

foreign import javascript unsafe "Mspiff.setCookie($1)"
 setCookie :: JSString -> IO ()

foreign import javascript unsafe "Mspiff.getCookie()"
 getCookie :: IO JSString

foreign import javascript unsafe "console.log($1)"
 log_ :: JSVal -> IO ()

foreign import javascript unsafe "eventCallback = $1"
 setEventCallback :: Callback (JSVal -> IO ()) -> IO ()

turnOffSpinner :: IO ()
turnOffSpinner = do
  node <- select "#loading"
  void $ setCss "visibility" "hidden" node

log :: String -> IO ()
log x = log_ $ toJSString ("HS: " <> x)

idFor :: Screening -> JSString
idFor s = pack $ "#screening-" <> show (screeningId s)

updateSchedule ::
  ScreeningMap ->
  Catalog ->
  MVar ScheduleState ->
  JSVal ->
  IO ()
updateSchedule smap Catalog{..} _ sid =
  forM_ (M.lookup (fromJSInt sid) smap) $ \s -> do
    update s
    tid <- myThreadId
    log $ "Clicked" <> show s <> " from thread " <> show tid

  where
    update s = do
      setColor "green" s
      mapM_ (setColor "orange") (others s)
      mapM_ (setColor "red") (overlapping s)
    nodeFor s = select (idFor s) >>= parent >>= parent
    setColor c s = nodeFor s >>= setCss "background-color" c

hsToJs :: ToJSON a => a -> JSString
hsToJs = pack . T.unpack . LT.toStrict . decodeUtf8 . encode

jsToHs :: FromJSON a => JSString -> Maybe a
jsToHs = decode' . encodeUtf8 . LT.fromStrict . T.pack . unpack

main :: IO ()
main = do
  let
    Just catalog = loadCatalog (LBS.fromStrict catalogJson)
    ss = screenings catalog
    visData = buildVisData catalog

    timelineData = DL.zip [0.. ] (hsToJs <$> visData)
    screeningMap = M.fromList $ (screeningId &&& id) <$> ss
  cookie <- jsToHs <$> getCookie
  log $ "C1: " <> show cookie
  mvar <- newMVar (maybe M.empty (fromPersistState screeningMap) cookie)

  mapM_ (uncurry renderDayTimeline) (DL.take 3 timelineData)
  turnOffSpinner
  let
    callback = updateSchedule screeningMap catalog mvar
  setEventCallback =<< syncCallback1 ContinueAsync callback
  setEventHandler



