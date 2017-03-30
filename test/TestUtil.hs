
module TestUtil where

import qualified Data.ByteString.Lazy as BS
import qualified Data.List as DL
import Test.QuickCheck hiding (Success)
import Data.Maybe
import System.IO.Unsafe
import Mspiff.Model
import Mspiff.Loader
import Mspiff.Scheduler

newtype ArbFilm = ArbFilm Film deriving (Show, Eq)
films_ :: [Film]
screenings_ :: [Screening]
Just (Catalog films_ screenings_) =
  loadCatalog $ unsafePerformIO (BS.readFile "data/catalog")

instance Arbitrary ArbFilm
  where
    arbitrary = do
      i <- choose (0, length films_ - 1)
      return $ ArbFilm (films_ !! i)

arbitraryDistinctList :: (Arbitrary a, Eq a) => Int -> Gen [a]
arbitraryDistinctList count = do
  let
    getOne list = do
      f <- arbitrary
      if f `elem` list then getOne list else return f
    build c list = do
      f <- getOne list
      if c == 0
        then return list
        else build (c -1) (f:list)
  build (min count (DL.length films_)) []

newtype FilmList = FilmList {fromFilmList :: [ArbFilm]} deriving Show
instance Arbitrary FilmList
  where
    arbitrary = FilmList <$> arbitraryDistinctList 20

newtype ArbScreening = ArbScreening {unArb :: Screening} deriving (Eq, Show)
newtype DisjointList = DisjointList {fromScreeningList :: [ArbScreening]}
  deriving Show

instance Arbitrary DisjointList
  where
    arbitrary = DisjointList <$> getOneDisjoint

instance Arbitrary ArbScreening
  where
    arbitrary = do
      i <- choose (0, length screenings_ - 1)
      return $ ArbScreening (screenings_ !! i)

getOneDisjoint :: Gen [ArbScreening]
getOneDisjoint = do
  l <- arbitraryDistinctList 20
  if disjoint (unArb <$> l) then return l else getOneDisjoint

fs :: ScreeningId -> Screening
fs sid = fromJust $ DL.find ((==sid) . screeningId) screenings_
s325, s326, s288 :: Screening
s325 = fs 325
s326 = fs 326
s288 = fs 288

