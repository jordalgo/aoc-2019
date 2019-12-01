module Day2
    ( getGroup
    , getCheckSum
    ) where

import qualified Data.List as List

getGroup :: String -> [Int]
getGroup = map length . List.group . List.sort

getCount :: Int -> Int -> [Int] -> Int
getCount x acc xs = if (x `elem` xs) then acc + 1 else acc

getPairCount = getCount 2
getTripleCount = getCount 3

getCheckSum :: [String] -> Int
getCheckSum list =
  let (twos, threes) = foldl (\(a, b) xs -> (getPairCount a xs, getTripleCount b xs)) (0, 0) (map getGroup list)
  in twos * threes
