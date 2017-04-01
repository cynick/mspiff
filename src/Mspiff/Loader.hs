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
tieOthers = DL.foldr tie [] . NE.toList
  where
    findOthers :: Screening -> [Screening]
    findOthers s = filmScreenings (filmOf s) \\ [s]
    tie s acc
      | s `elem` acc = acc
      | otherwise =
          case findOthers s of
            [o1,o2,o3,o4] ->
              let s'  = s {others = os' \\ [s']}
                  o1' = o1 {others = os' \\ [o1']}
                  o2' = o2 {others = os' \\ [o2']}
                  o3' = o3 {others = os' \\ [o3']}
                  o4' = o4 {others = os' \\ [o4']}
                  os' = [s',o1',o2',o3',o4']
              in os' ++ acc
            [o1,o2,o3] ->
              let s'  = s {others = os' \\ [s']}
                  o1' = o1 {others = os' \\ [o1']}
                  o2' = o2 {others = os' \\ [o2']}
                  o3' = o3 {others = os' \\ [o3']}
                  os' = [s',o1',o2',o3']
              in os' ++ acc
            [o1,o2] ->
              let s'  = s {others = [o1',o2']}
                  o1' = o1 {others = [s',o2']}
                  o2' = o2 {others = [s',o1']}
              in s' : o1' : o2' : acc
            [o1] ->
              let s' = s {others = [o1']}
                  o1' = o1 {others = [s']}
              in s' : o1' : acc

            _ -> s : acc

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

    films0 :: [Film]
    films0 = computeDeps _films
      where
        computeDeps = fmap set
        schedule = Schedule screenings0
        set f = f { filmScreenings = screeningsFor schedule f }

    screenings :: [Screening]
    screenings =
      DL.sort $ DL.concat $ tieOthers <$> NE.groupAllWith scFilmId screenings0

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

