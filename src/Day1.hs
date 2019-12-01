module Day1
    ( getResult
    , getRepeatsResult
    , firstDup
    , part2
    ) where

import qualified Data.Set as Set

getItem :: String -> Int
getItem = read . filter (/= '+')

getResult :: [String] -> Int
getResult = sum . map getItem

getRepeats :: [String] -> Int -> Set.Set Int -> Int
getRepeats list start set =
  let (x:xs) = list
      next = start + (getItem x)
      in if (Set.member next set)
        then next
        else getRepeats xs next (Set.insert next set)

getRepeatsResult :: [String] -> Int
getRepeatsResult list = getRepeats (cycle list) 0 Set.empty

firstDup :: Ord a => [a] -> Maybe a
firstDup = go Set.empty
  where go seen [] = Nothing
        go seen (x:xs) | (Set.member x seen) = Just x
                       | otherwise = go (Set.insert x seen) xs

part2 :: [String] -> Maybe Int
part2 = firstDup . scanl (+) 0 . cycle . map getItem
