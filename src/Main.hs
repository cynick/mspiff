
module Main where

import Mspiff.Model
import Mspiff.Loader
import Mspiff.Scheduler
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import qualified Data.List as DL
import System.Environment
import Data.Array
import Control.Monad
main :: IO ()
main = do
  (c:_) <- getArgs
  let w = Schedule (elems screenings)
  forever $ do
    f <- replicateM (readInt c) (generate arbitrary) :: IO [Film]
    case DL.take 1 (viewableSchedulesFor' w f) of
      [x] -> print (showtimesForSchedule x)
      _ -> print "NONE"
