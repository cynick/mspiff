module Mspiff.Loader
    (loadCatalog)
where

import Prelude
import Control.Arrow
import qualified Data.ByteString.Lazy as BS
import Data.Aeson hiding (Array)
import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.List as DL
import qualified Data.Map.Strict as M

import Mspiff.Model
import Mspiff.ModelUtil

loadCatalog :: BS.ByteString -> Maybe Catalog
loadCatalog = maybe Nothing update . decode'

screeningsFor :: [Screening] -> Film -> [Screening]
screeningsFor ss f = DL.sort $ filter (filt f) ss
  where filt film sc = scFilmId sc == filmId film

update :: Catalog -> Maybe Catalog
update (Catalog venues _films _screenings _ _) = Just Catalog{..}
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
        set f = f { filmScreenings = screeningsFor screenings f }
    screeningMap = M.fromList $ (screeningId &&& id) <$> screenings
    filmMap = M.fromList $ (filmId &&& id) <$> films

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


