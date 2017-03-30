
module Main where

import qualified Data.Text.Lazy.IO as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS

import qualified Data.Text as T
import Data.FileEmbed

-- import JavaScript.JQuery
-- import JavaScript.JQuery.Internal

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
  T.putStrLn html

{-

  b <- select "body"
  append "<div class='mouse'></div>" b
  m <- select ".mouse"
  mousemove (handler m) def b


handler :: JQuery -> Event -> IO ()
handler m e = do
  x <- pageX e
  y <- pageY e
  setText (T.pack $ show (x,y)) m
  return ()

-}
