module Mspiff.Loader where

import Prelude

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as M
import Data.Aeson hiding (Array)
import Data.List
import Data.List.Split
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.List as DL
import Data.Maybe
import Data.Array
import Data.These
import Control.Monad
import System.IO.Unsafe

import Mspiff.Model

instance FromJSON Film where
  parseJSON (Object v) =
    Film <$>
      v .: "filmId" <*>
      v .: "filmTitle" <*>
      pure []

  parseJSON _ = error "invalid film json"

instance FromJSON Screening where
  parseJSON (Object v) =
    Screening <$>
      v .: "scFilmId" <*>
      v .: "screeningId" <*>
      pure [] <*>
      pure Nothing <*>
      v .: "screeningTime" <*> -- seconds since Epoch
      ((60*) <$> v .: "duration" ) <*> -- duration is given in minutes
      v .: "screen"

  parseJSON _ = error "invalid screening json"

load :: FromJSON a => FilePath -> IO a
load path = do
  putStrLn $ "Loading " ++ path
  raw <- BS.readFile path
  case decode raw of
    Just r ->
      case fromJSON r of
        Success a -> do
          putStrLn $ "Finished loading " ++ path
          return a
        Error s -> error s
    Nothing -> error "Failed to parse"

loadFilms :: IO [Film]
loadFilms = load "data/films"

loadScreenings :: IO [Screening]
loadScreenings = load "data/screenings"

toArray :: IO [e] -> Array Int e
toArray m = unsafePerformIO $ do
  l <- m
  return $ array (0, DL.length l -1) (DL.zip [0..] l)

ws = Schedule (elems screenings)

screenings :: Array Int Screening
screenings = toArray ((computeDeps . DL.sort) <$> loadScreenings)
  where
    computeDeps ss =
      let
        findOverlapping ss s =
          fmap snd . filter overlaps . fmap (s,) $ (ss \\ [s])
        findOther ss s = find (isOther s) ss

        setDeps ss s =
          s { overlapping = findOverlapping ss s
            , otherScreening = findOther ss s
            }
        tie (a,b) acc =
          (a {otherScreening = Just b}) : (b {otherScreening = Just a}) : acc
        ss' = (setDeps ss) <$> ss
        ss'' = do
          (a:xs) <- tails ss'
          b <- xs
          let b' = findOther ss' a
          guard (Just b == b')
          return $! (a,b)
      in foldr tie [] ss''

isOther s s' = s /= s' && scFilmId s' == scFilmId s
findOther ss s = find (isOther s) ss

tie (a,b) acc =
  (a {otherScreening = Just b}) : (b {otherScreening = Just a}) : acc
sps = do
  (a:xs) <- tails (elems screenings)
  b <- xs
  let b' = findOther screenings a
  guard (Just b == b')
  return $! (a,b)

screeningsFor :: WholeSchedule -> Film -> [Screening]
screeningsFor s f = sort $ filter (filt f) (scheduleScreenings s)
  where filt film screening = scFilmId screening == filmId film

after :: Screening -> Screening -> Bool
after a b = showtime a > showtime b + duration b

overlaps :: (Screening, Screening) -> Bool
overlaps (a, b) = not (a `after` b || b `after` a)

films :: Array Int Film
films = toArray (computeDeps <$> loadFilms)
  where
    computeDeps fs =
      let
        schedule = Schedule (elems screenings)
        set f = f { filmScreenings = screeningsFor schedule f }
      in set <$> fs

filmOf :: Screening -> Film
filmOf s = fromJust $ DL.find (\f -> filmId f == scFilmId s) (elems films)
