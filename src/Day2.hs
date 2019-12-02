module Day2
    ( runProgram
    , getResult
    , fixInput
    ) where

import qualified Data.List.Index as Index
import qualified Data.List.Split as Split

fixInput :: [Int] -> [Int]
fixInput = (Index.setAt 2 2) . (Index.setAt 1 12)

runProgram :: Int -> [Int] -> Int
runProgram position xs
  | code == 1 = runProgram fourth (Index.setAt third (first + second) xs)
  | code == 2 = runProgram fourth (Index.setAt third (first * second) xs)
  | otherwise = head xs
  where
    code = xs !! position
    first = xs !! (xs !! (position + 1))
    second = xs !! (xs !! (position + 2))
    third = xs !! (position + 3)
    fourth = position + 4

getResult :: String -> Int
getResult = runProgram 0 . fixInput . map read . Split.splitOn ","
