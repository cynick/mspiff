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

turnOffSpinner :: IO ()
turnOffSpinner = do
  node <- select "#loading"
  void $ setCss "visibility" "hidden" node

log :: String -> IO ()
log x = log_ $ toJSString ("HS: " <> x)

idFor :: Screening -> JSString
idFor s = pack $ "#screening-" <> show (screeningId s)

redraw :: Catalog -> ScheduleState -> ScheduleState -> IO ()
redraw _ old = do
  log "REDRAW"
  mapM_ (mapM_ go . unGroup) . M.elems
  where
    go :: MarkedScreening -> IO ()
    go MarkedScreening{..} = do
      case status of
        Scheduled -> do
          setColor "green" screening
          mapM_ (setColor "orange") (others screening)
          mapM_ (setColor "darkgrey") (overlapping screening)

    nodeFor s = select (idFor s) >>= parent >>= parent
    setColor c s = nodeFor s >>= setCss "background-color" c

findScreening k = M.lookup (fromJSInt k)

handleScreeningCmd ::
  (Screening -> Command) ->
  ScreeningMap ->
  Catalog ->
  TBMChan Command ->
  JSVal ->
  IO ()
handleScreeningCmd cmd smap Catalog{..} chan sid = do
  forM_ ms $ \s -> do
    let cmd' = cmd s
    log $ "CMD: " ++ show cmd'
    atomically (writeTBMChan chan cmd')
  where
    ms = findScreening sid smap

handleCmd ::
  Command ->
  ScreeningMap ->
  Catalog ->
  TBMChan Command ->
  JSVal ->
  IO ()
handleCmd cmd smap Catalog{..} chan sid = do
  log $ "CMD: " ++ show cmd
  atomically (writeTBMChan chan cmd)

hsToJs :: ToJSON a => a -> JSString
hsToJs = pack . T.unpack . LT.toStrict . decodeUtf8 . encode

jsToHs :: FromJSON a => JSString -> Maybe a
jsToHs = decode' . encodeUtf8 . LT.fromStrict . T.pack . unpack

setCallback screeningMap catalog chan handler setter =
  setter =<< syncCallback1 ContinueAsync callback
  where
    callback = handler screeningMap catalog chan

main :: IO ()
main = do
  let
    Just catalog = loadCatalog (LBS.fromStrict catalogJson)
    ss = screenings catalog
    visData = buildVisData catalog

    timelineData = DL.zip [0.. ] (hsToJs <$> visData)
    screeningMap = M.fromList $ (screeningId &&& id) <$> ss
  persistState <- jsToHs <$> getCookie
  let
    state = maybe M.empty (fromPersistState screeningMap) persistState
  log $ "C1: " <> show persistState
  chan <- newTBMChanIO 5

  mapM_ (uncurry renderDayTimeline) (DL.take 6 timelineData)
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

  zipWithM_ (setCallback screeningMap catalog chan) handlers setHandlers
  startSchedulerLoop chan (redraw catalog)
  setEventHandlers
  postInit

