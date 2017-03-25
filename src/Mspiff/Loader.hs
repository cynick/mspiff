module Mspiff.Loader where

import Prelude

import qualified Data.ByteString.Lazy as BS

import Data.Aeson hiding (Array)
import Data.List
import qualified Data.List as DL
import Data.Maybe
import Data.Array
import System.IO.Unsafe

import Mspiff.Model

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

loadList :: IO [e] -> [e]
loadList = unsafePerformIO

screenings0 :: [Screening]
screenings0 = (computeDeps . DL.sort) (loadList loadScreenings)
  where
    computeDeps ss =
      let
        findOverlapping s =
          fmap snd . filter overlaps . fmap (s,) $ (ss \\ [s])

        setOverlapping s =
          s { overlapping = findOverlapping s }
        ss' = setOverlapping <$> ss

      in ss'

films0 :: [Film]
films0 = computeDeps (loadList loadFilms)
  where
    computeDeps fs =
      let
        schedule = Schedule screenings0
        set f = f { filmScreenings = screeningsFor schedule f }
      in set <$> fs

filmOf :: Screening -> Film
filmOf s = fromJust $ DL.find (\f -> filmId f == scFilmId s) films0

screenings :: [Screening]
screenings = DL.sort $ DL.nub $ foldr tie [] screenings0
  where
    findOtherScreening s = find (isOther s) (filmScreenings (filmOf s))
    tie s acc
      | s `elem` acc = acc
      | otherwise =
          case findOtherScreening s of
            Just other ->
              let s' = s {otherScreening = Just other'}
                  other' = other {otherScreening = Just s'}
              in s' : other' : acc
            _ -> s : acc

films :: [Film]
films = set <$> films0
  where
    schedule = Schedule screenings
    set f = f { filmScreenings = screeningsFor schedule f }

screeningsFor :: WholeSchedule -> Film -> [Screening]
screeningsFor s f = sort $ filter (filt f) (scheduleScreenings s)
  where filt film sc = scFilmId sc == filmId film

after :: Screening -> Screening -> Bool
after a b = showtime a > showtime b + duration b

overlaps :: (Screening, Screening) -> Bool
overlaps (a, b) = not (a `after` b || b `after` a)



