module Mspiff.Loader where

import Prelude

import qualified Data.ByteString.Lazy as BS
import Data.Aeson hiding (Array)
import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.List as DL

import Mspiff.Model

filmOf :: Screening -> Film
filmOf = undefined

tieOthers :: NE.NonEmpty Screening -> [Screening]
tieOthers = go . NE.toList
  where
    go [s1,s2,s3,s4,s5] =
      let
        s1' = s1 {others = s' \\ [s1']}
        s2' = s2 {others = s' \\ [s2']}
        s3' = s3 {others = s' \\ [s3']}
        s4' = s4 {others = s' \\ [s4']}
        s5' = s5 {others = s' \\ [s5']}
        s' = [s1', s2', s3', s4', s5']
      in s'
    go [s1,s2,s3,s4] =
      let
        s1' = s1 {others = s' \\ [s1']}
        s2' = s2 {others = s' \\ [s2']}
        s3' = s3 {others = s' \\ [s3']}
        s4' = s4 {others = s' \\ [s4']}
        s' = [s1', s2', s3', s4']
      in s'
    go [s1,s2,s3] =
      let
        s1' = s1 {others = s' \\ [s1']}
        s2' = s2 {others = s' \\ [s2']}
        s3' = s3 {others = s' \\ [s3']}
        s' = [s1', s2', s3']
      in s'
    go [s1,s2] =
      let
        s1' = s1 {others = [s2']}
        s2' = s2 {others = [s1']}
        s' = [s1', s2']
      in s'
    go [s1] = [s1]
    go x = error $ "unhandled case in tie others " ++ show x

loadCatalog :: BS.ByteString -> Maybe Catalog
loadCatalog = maybe Nothing update . decode'

update :: Catalog -> Maybe Catalog
update (Catalog _films _screenings) = Just (Catalog films screenings)
  where
    screenings0 :: [Screening]
    screenings0 = computeDeps . DL.sort $ _screenings
      where
        computeDeps ss = setOverlapping <$> ss
          where
            findOverlapping s =
              fmap snd . filter overlaps . fmap (s,) $ (ss \\ [s])

            setOverlapping s =
              s { overlapping = findOverlapping s }

    screenings :: [Screening]
    screenings =
      DL.sort $ DL.concat $ tieOthers <$> NE.groupAllWith scFilmId screenings0

    films :: [Film]
    films = set <$> _films
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

