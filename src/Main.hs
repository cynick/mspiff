{-# LANGUAGE JavaScriptFFI, CPP #-}
module Main where

import Prelude hiding (log)
import Control.Monad
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
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
import Data.JSString.Text
import Data.Conduit.TMChan
import Data.Aeson
import Data.Conduit
import Data.Maybe
import Data.Text.Lazy.Encoding
import Data.FileEmbed
import Data.Default
import Data.Monoid
import Data.Tuple
import Data.JSString hiding (find)
import JavaScript.JQuery
import JavaScript.JQuery.Internal

import Lucid

import Mspiff.Model
import Mspiff.ModelUtil
import Mspiff.Loader
import Mspiff.Scheduler
import Mspiff.Html
import Mspiff.Vis
import Mspiff.Scrape

catalogJson :: BS.ByteString
catalogJson = $(embedFile "data/catalog")

foreign import javascript unsafe "Mspiff.renderDayTimeline($1,$2,$3)"
 renderDayTimeline :: Int -> Int -> JSString -> IO ()

foreign import javascript unsafe "Mspiff.setEventHandlers()"
 setEventHandlers :: IO ()

foreign import javascript unsafe "Mspiff.setCookie($1)"
 setCookie :: JSString -> IO ()

foreign import javascript unsafe "Mspiff.getCookie()"
 getCookie :: IO JSString

foreign import javascript unsafe "Mspiff.showBlurbModal($1,$2)"
 showBlurbModal :: JSString -> JSString -> IO ()

foreign import javascript unsafe "Mspiff.postInit()"
 postInit :: IO ()

foreign import javascript unsafe "console.log($1)"
 log_ :: JSVal -> IO ()

foreign import javascript unsafe "addScreening = $1"
 setAddScreening :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "ruleOutScreening = $1"
 setRuleOutScreening :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "pinScreening = $1"
 setPinScreening :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "unPinScreening = $1"
 setUnPinScreening :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "removeFilm = $1"
 setRemoveFilm :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "showBlurb = $1"
 setShowBlurb :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "clearState = $1"
 setClearState :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "redraw = $1"
 setRedraw :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "Mspiff.showControlsFor($1)"
 showControlsFor :: JSString -> IO ()

foreign import javascript unsafe "Mspiff.hideControlsFor($1)"
 hideControlsFor :: JSString -> IO ()

setSpinner visibility =
  select ".loading" >>= setCss "visibility" visibility >> return ()

turnOffSpinner :: IO ()
turnOffSpinner = setSpinner "hidden"

turnOnSpinner :: IO ()
turnOnSpinner = setSpinner "visible"

spinBracket =
  ( log "SCHEDULER START" >> turnOnSpinner
  , turnOffSpinner >> log "SCHEDULER DONE")

log :: String -> IO ()
log x = log_ $ toJSString ("HS: " <> x)

idFor :: Screening -> JSString
idFor s = pack $ "#screening-" <> show (screeningId s)

redraw :: Catalog -> ScheduleState -> ScheduleState -> IO ()
redraw _ old new = do
  mapM_ updateOld oldMS
  mapM_ updateNew newMS
  setCookie (hsToJs (toPersistState new))
  log $ "OLD: " ++ show oldMS
  log $ "NEW: " ++ show newMS
  where
    newMS = DL.sort $ join (M.elems new)
    oldMS = DL.sort $ join (M.elems old)
    updateNew ms@MarkedScreening{..} = do
      showControlsFor (idFor screening)
      setScreeningStatus screening status
      setPinStatus pinned screening
    updateOld ms@MarkedScreening{..} =
       when ( ms `notElem` newMS ) $ do
        hideControlsFor (idFor screening)
        setPinStatus Unpinned screening
    setScreeningStatus screening stat = do
      node <- select (idFor screening <> " .screening-status")
      setStatusColor stat node
      setStatusTitle stat node
    setStatusColor status = setCss "background-color" color
      where
        color =
          case status of
            Unscheduled -> "blue"
            Scheduled -> "green"
            Impossible -> "red"
            RuledOut -> "darkgrey"
            OtherPinned -> "lightgrey"
            OtherScheduled -> "orange"
    setStatusTitle status = setAttr "title" title
      where
        title =
          case status of
            Unscheduled -> "Unscheduled"
            Scheduled -> "Scheduled"
            Impossible -> "Screening is Impossible"
            RuledOut -> "Ruled Out"
            OtherPinned -> "Other Screening is Pinned"
            OtherScheduled -> "Other Screening is Scheduled"

findScreening = M.lookup . fromJSInt

setPinStatus :: Pinned -> Screening -> IO ()
setPinStatus pinned s = do
  let
    icon = ("fa-circle-o","fa-circle")
    stat = ("unpinned","pinned")
    ((oldIcon, newIcon),(oldStat,newStat),title) =
      if pinned == Pinned
        then (icon,stat,"Unpin Screening")
        else (swap icon, swap stat, "Pin Screening")

  el <- find ".pin-screening" =<< select (idFor s)
  setAttr "title" title el >>= removeClass oldStat >>= addClass newStat
  find "a .fa" el >>= removeClass oldIcon >>= addClass newIcon

  return ()

showBlurbFor :: Film -> IO ()
showBlurbFor film = do
  blurb <- fetchBlurbFor film
  maybe (return ()) showBlurb blurb
  where
    showBlurb Blurb{..} = do
      log "SHOWING BLURB"
      showBlurbModal (textToJSString blurbImage) (textToJSString blurbText)

handleScreeningCmd ::
  (Screening -> Command) ->
  Catalog ->
  TBMChan Command ->
  JSVal ->
  IO ()
handleScreeningCmd cmd cat@Catalog{..} chan sid =
  forM_ ms $ \s -> do
    let cmd' = cmd s
    log $ "CMD: " ++ show cmd'
    handle cmd'
  where
    ms = findScreening sid screeningMap
    sendCmd = atomically . writeTBMChan chan
    handle (ShowBlurb s) = maybe (return ()) showBlurbFor (filmOf s)
    handle cmd = sendCmd cmd
    filmOf s = M.lookup (scFilmId s) filmMap

handleCmd ::
  Command ->
  Catalog ->
  TBMChan Command ->
  JSVal ->
  IO ()
handleCmd cmd _ chan sid = do
  log $ "CMD: " ++ show cmd
  atomically (writeTBMChan chan cmd)

hsToJs :: ToJSON a => a -> JSString
hsToJs = pack . T.unpack . LT.toStrict . decodeUtf8 . encode

jsToHs :: FromJSON a => JSString -> Maybe a
jsToHs = decode' . encodeUtf8 . LT.fromStrict . T.pack . unpack

setCallback cat@Catalog{..} chan handler setter =
  setter =<< syncCallback1 ContinueAsync callback
  where
    callback = handler cat chan

setupHandlers :: Catalog -> TBMChan Command -> IO ()
setupHandlers catalog chan = do
  let
    handlers =
      [ handleScreeningCmd Add
      , handleScreeningCmd RuleOut
      , handleScreeningCmd Pin
      , handleScreeningCmd UnPin
      , handleScreeningCmd RemoveFilm
      , handleScreeningCmd ShowBlurb
      , handleCmd Clear
      , handleCmd Redraw
      ]
    setHandlers =
      [ setAddScreening
      , setRuleOutScreening
      , setPinScreening
      , setUnPinScreening
      , setRemoveFilm
      , setShowBlurb
      , setClearState
      , setRedraw
      ]
  zipWithM_ (setCallback catalog chan) handlers setHandlers

main :: IO ()
main = do
  let
    Just catalog@Catalog{..} = loadCatalog (LBS.fromStrict catalogJson)
    visData = buildVisData catalog

    timelineData = DL.zip [0.. ] (hsToJs <$> visData)
  persistState <- jsToHs <$> getCookie
  log $ show (hsToJs persistState)
  let state = maybe M.empty (fromPersistState screeningMap) persistState
  chan <- newTBMChanIO 5
  setupHandlers catalog chan
  startSchedulerLoop chan catalog (redraw catalog) spinBracket state
  mapM_ (uncurry (renderDayTimeline (DL.length timelineData))) timelineData
  setEventHandlers
  postInit
  turnOffSpinner

