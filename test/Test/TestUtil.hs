
module Test.TestUtil where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as DL
import Test.QuickCheck hiding (Success)
import Data.Maybe
import Mspiff.Model
import Mspiff.Loader
import Mspiff.Scheduler
import Data.FileEmbed
import Test.HUnit

at :: Assertion
at = assertBool "" True

newtype ArbFilm = ArbFilm Film deriving (Show, Eq)

catalogJson :: BS.ByteString
catalogJson = $(embedFile "data/catalog")

catalog :: Catalog
catalog = fromJust (loadCatalog (LBS.fromStrict catalogJson))

films_ :: [Film]
films_ = films catalog

screenings_ :: [Screening]
screenings_ = screenings catalog

ss :: [Screening]
ss = screenings catalog

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

