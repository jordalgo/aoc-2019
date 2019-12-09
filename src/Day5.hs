module Day5
    ( getPart1
    ) where

import Data.List
import Data.List.Index
import Data.List.Split

getOpCode x = x `mod` 100
getParameterX position x = (floor (fromIntegral (x `div` (10^(position + 1))))) `mod` 10
getParameter1 = getParameterX 1
getParameter2 = getParameterX 2
getParameter3 = getParameterX 3

runProgram :: Int -> Int -> [Int] -> Int
runProgram input position xs
  | code == 1 = runProgram 0 (position + 4) (setAt param3 (first + second) xs)
  | code == 2 = runProgram 0 (position + 4) (setAt param3 (first * second) xs)
  | code == 3 = runProgram 0 (position + 2) (setAt param1 input xs)
  | code == 4 = runProgram (xs !! param1) (position + 2) xs
  | otherwise = input
  where
    instruction = xs !! position
    code = getOpCode instruction
    param1Mode = getParameter1 instruction
    param2Mode = getParameter2 instruction
    param3Mode = getParameter3 instruction
    param1 = xs !! (position + 1)
    param2 = xs !! (position + 2)
    param3 = xs !! (position + 3)
    param4 = xs !! (position + 4)
    first = if (param1Mode == 1) then param1 else (xs !! param1)
    second = if (param2Mode == 1) then param2 else (xs !! param2)

getPart1 :: String -> Int
getPart1 x = runProgram 1 0 (map read (splitOn "," x))
