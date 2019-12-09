module Day5
    ( getPart1
    , getPart2
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
  | code == 1 = runProgram 0 posP4 (setAt param3 (first + second) xs)
  | code == 2 = runProgram 0 posP4 (setAt param3 (first * second) xs)
  | code == 3 = runProgram 0 posP2 (setAt param1 input xs)
  | code == 4 = runProgram (xs !! param1) posP2 xs
  | code == 5 = runProgram 0 (if (first > 0) then second else posP3) xs
  | code == 6 = runProgram 0 (if (first == 0) then second else posP3) xs
  | code == 7 = runProgram 0 posP4 (setAt param3 (if (first < second) then 1 else 0) xs)
  | code == 8 = runProgram 0 posP4 (setAt param3 (if (first == second) then 1 else 0) xs)
  | otherwise = input
  where
    instruction = xs !! position
    code = getOpCode instruction
    param1Mode = getParameter1 instruction
    param2Mode = getParameter2 instruction
    param3Mode = getParameter3 instruction
    posP2 = position + 2
    posP3 = position + 3
    posP4 = position + 4
    param1 = xs !! (position + 1)
    param2 = xs !! posP2
    param3 = xs !! posP3
    first = if (param1Mode == 1) then param1 else (xs !! param1)
    second = if (param2Mode == 1) then param2 else (xs !! param2)

runProgramWithInput :: Int -> String -> Int
runProgramWithInput initial = runProgram initial 0 . map read . splitOn ","

getPart1 :: String -> Int
getPart1 = runProgramWithInput 1

getPart2 :: String -> Int
getPart2 = runProgramWithInput 5
