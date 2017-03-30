module Mspiff.Loader where

import Prelude

import qualified Data.ByteString.Lazy as BS
import Data.Aeson hiding (Array)
import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.List as DL
import Data.Maybe

import Mspiff.Model

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
    screenings = DL.sort $ DL.concat $ tie <$> NE.groupAllWith scFilmId screenings0
      where
        tie :: NE.NonEmpty Screening -> [Screening]
        tie ss = fmap set list
          where
            list = NE.toList ss
            set s = s { others = list \\ [s] }

    films :: [Film]
    films = set <$> films0
      where
        schedule = Schedule screenings
        set f = f { filmScreenings = screeningsFor schedule f }

{-                
filmOf :: Screening -> Film
filmOf s = fromJust $ DL.find (\f -> filmId f == scFilmId s) films
-}

screeningsFor :: WholeSchedule -> Film -> [Screening]
screeningsFor s f = sort $ filter (filt f) (scheduleScreenings s)
  where filt film sc = scFilmId sc == filmId film

after :: Screening -> Screening -> Bool
after a b = showtime a > showtime b + duration b

overlaps :: (Screening, Screening) -> Bool
overlaps (a, b) = not (a `after` b || b `after` a)

