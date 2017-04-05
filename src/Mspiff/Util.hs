module Mspiff.Util where

import Data.Maybe
import qualified Data.List as DL
import Data.Text

type Name = Text


findFor :: Eq b => [a] -> (a -> b) -> b -> a
findFor xs f name = fromJust (DL.find filt xs)
  where filt x = f x == name

