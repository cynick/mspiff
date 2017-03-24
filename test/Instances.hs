
module Instances where

import qualified Data.List as DL
import Data.Array
import Test.QuickCheck hiding (Success)

import Mspiff.Model
import Mspiff.Loader
import Mspiff.Scheduler

instance Arbitrary Film
  where
    arbitrary = do
      i <- choose (0, length films - 1)
      return (films ! i)

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
  build (min count (DL.length films)) []

newtype FilmList = FilmList {fromFilmList :: [Film]} deriving Show
instance Arbitrary FilmList
  where
    arbitrary = FilmList <$> arbitraryDistinctList 20

newtype DisjointList = DisjointList {fromScreeningList :: [Screening]}
  deriving Show

getOneDisjoint :: Gen [Screening]
getOneDisjoint = do
  l <- arbitraryDistinctList 20
  if disjoint l then return l else getOneDisjoint

instance Arbitrary DisjointList
  where
    arbitrary = DisjointList <$> getOneDisjoint

instance Arbitrary Screening
  where
    arbitrary = do
      i <- choose (0, length screenings - 1)
      return (screenings ! i)
