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
import Data.Conduit.TMChan
import Data.Aeson
import Data.Conduit
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
import Mspiff.ModelUtil
import Mspiff.Loader
import Mspiff.Scheduler
import Mspiff.Html
import Mspiff.Vis

catalogJson :: BS.ByteString
catalogJson = $(embedFile "data/catalog")

foreign import javascript unsafe "Mspiff.renderDayTimeline($1,$2)"
 renderDayTimeline :: Int -> JSString -> IO ()

foreign import javascript unsafe "Mspiff.setEventHandlers()"
 setEventHandlers :: IO ()

foreign import javascript unsafe "Mspiff.setCookie($1)"
 setCookie :: JSString -> IO ()

foreign import javascript unsafe "Mspiff.getCookie()"
 getCookie :: IO JSString

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

foreign import javascript unsafe "clearState = $1"
 setClearState :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "Mspiff.showControlsFor($1)"
 showControlsFor :: JSString -> IO ()

foreign import javascript unsafe "Mspiff.hideControlsFor($1)"
 hideControlsFor :: JSString -> IO ()

turnOffSpinner :: IO ()
turnOffSpinner = do
  node <- select "#loading"
  void $ setCss "visibility" "hidden" node

log :: String -> IO ()
log x = log_ $ toJSString ("HS: " <> x)

idFor :: Screening -> JSString
idFor s = pack $ "#screening-" <> show (screeningId s)

update :: Catalog -> ScheduleState -> ScheduleState -> IO ()
update _ old new = do
  log $ "REDRAW: " ++ show new
  mapM_ updateOld oldMS
  mapM_ updateNew newMS
  setCookie (hsToJs (toPersistState new))
  log $ "OLD: " ++ show (screeningId . screening <$> oldMS)
  log $ "NEW: " ++ show (screeningId . screening <$> newMS)
  where
    newMS = DL.sort $ join (M.elems new)
    oldMS = DL.sort $ join (M.elems old)
    updateNew ms@MarkedScreening{..} = do
      showControlsFor (idFor screening)
      setColor screening $ case status of
        Scheduled -> "green"
        OtherScheduled -> "orange"
        RuledOut -> "darkgrey"
        Impossible -> "red"
        _ -> "blue"
    updateOld ms@MarkedScreening{..} = do
       when ( ms `notElem` newMS ) $ do
        setColor screening "rgb(212,221,246)"
        hideControlsFor (idFor screening)
    nodeFor s = select (idFor s) >>= parent >>= parent
    setColor s c = nodeFor s >>= setCss "background-color" c >> return ()

findScreening k = M.lookup (fromJSInt k)

handleScreeningCmd ::
  (Screening -> Command) ->
  Catalog ->
  TBMChan Command ->
  JSVal ->
  IO ()
handleScreeningCmd cmd cat@Catalog{..} chan sid = do
  forM_ ms $ \s -> do
    let cmd' = cmd s
    log $ "CMD: " ++ show cmd'
    atomically (writeTBMChan chan cmd')
  where
    ms = findScreening sid screeningMap

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

main :: IO ()
main = do
  let
    Just catalog@Catalog{..} = loadCatalog (LBS.fromStrict catalogJson)
    visData = buildVisData catalog

    timelineData = DL.zip [0.. ] (hsToJs <$> visData)
  persistState <- jsToHs <$> getCookie
  let
    state = maybe M.empty (fromPersistState screeningMap) persistState
  chan <- newTBMChanIO 5

  mapM_ (uncurry renderDayTimeline) timelineData
  turnOffSpinner

  let
    handlers =
      [ handleScreeningCmd Add
      , handleScreeningCmd RuleOut
      , handleScreeningCmd Pin
      , handleScreeningCmd UnPin
      , handleScreeningCmd RemoveFilm
      , handleCmd Clear
      ]
    setHandlers =
      [ setAddScreening
      , setRuleOutScreening
      , setPinScreening
      , setUnPinScreening
      , setRemoveFilm
      , setClearState
      ]

  zipWithM_ (setCallback catalog chan) handlers setHandlers
  update catalog state state
  startSchedulerLoop chan catalog (update catalog) state
  setEventHandlers
  postInit

