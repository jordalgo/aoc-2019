module Day2
    ( runProgram
    , getResult
    , fixInput
    ) where

import qualified Data.List.Index as Index
import qualified Data.List.Split as Split

fixInput :: Int -> Int -> [Int] -> [Int]
fixInput x y = (Index.setAt 2 y) . (Index.setAt 1 x)

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

runProgramWithInputs :: Int -> Int -> [Int] -> Int
runProgramWithInputs noun verb = runProgram 0 . (fixInput noun verb)

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs else []

getResult1 :: String -> Int
getResult1 = runProgram 0 . (fixInput 12 2) . map read . Split.splitOn ","

getResult :: String -> Int
getResult strInput =
  let input = map read (Split.splitOn "," strInput)
      final = reverse (takeWhileInclusive (\(x, y) -> (runProgramWithInputs x y input) /= 19690720) [ (i,j) | i <- [0..99], j <- [0..99] ])
      tuple = head final
      in ((fst tuple) * 100) + (snd tuple)
