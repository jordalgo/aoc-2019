module Day8
    ( getPart1
    , getPart2
    ) where

import Data.List
import Data.List.Index
import Data.List.Split

getCount :: Int -> [Int] -> Int
getCount x = length . filter (x==)

getMaybeInt :: Maybe Int -> Int
getMaybeInt (Just a) = a
getMaybeInt Nothing = 0

getPart1 :: String -> Int
getPart1 str = (getCount 1 foundLayer) * (getCount 2 foundLayer)
  where
  numInputs = map (\c -> read [c]) str
  layers = chunksOf (25 * 6) numInputs
  zeros = map (getCount 0) layers
  zeroIndex = elemIndex (minimum zeros) zeros
  foundLayer = layers !! (getMaybeInt zeroIndex)

getPart2 str = 5
