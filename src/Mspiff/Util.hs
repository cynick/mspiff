module Mspiff.Util where

import Data.Maybe
import qualified Data.List as DL
import Data.Text
import Data.Aeson
import qualified Data.Aeson as A
import Data.Aeson.Types
import Control.Monad

type Name = Text


findFor :: Eq b => [a] -> (a -> b) -> b -> a
findFor xs f name = fromJust (DL.find filt xs)
  where filt x = f x == name

stringToJson :: String -> Value
stringToJson = A.String . pack

enumToJson :: Enum a => a -> Value
enumToJson = stringToJson . show . fromEnum

jsonToEnum :: (Read a, Enum a) => String -> Value -> Parser a
jsonToEnum name = go >=> failIfNothing
  where go (String t) = case reads (unpack t) of
                          (p, ""):_ -> return (Just p)
                          _ -> return Nothing
        go x = fail $ "expected json value of " ++ name ++ " to be a string, "
                    ++ "got " ++ show x
        failIfNothing a =
            case a of Just a' -> return a'
                      Nothing -> fail $ "unexpected value for " ++ name
