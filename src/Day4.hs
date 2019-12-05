module Day4
    ( getPart1
    , getPart2
    ) where

import Data.List

areDigitsAsc :: String -> Bool
areDigitsAsc xs = sort xs == xs

getGroupLengths :: String -> [Int]
getGroupLengths = map length . group

hasSameAdjacentDigits :: String -> Bool
hasSameAdjacentDigits = any (>1) . getGroupLengths

hasDouble :: String -> Bool
hasDouble = any (2==) . getGroupLengths

getPart1Amount :: (Int, Int) -> Int
getPart1Amount (start, end) = length (filter (\x -> (areDigitsAsc x) && (hasSameAdjacentDigits x)) (map show [start..end]))

getRange :: [String] -> (Int, Int)
getRange input = ((nums !! 0), (nums !! 1))
  where
  nums = map read input

getPart1 :: [String] -> Int
getPart1 = getPart1Amount . getRange

getPart2Amount :: (Int, Int) -> Int
getPart2Amount (start, end) = length (filter (\x -> (areDigitsAsc x) && (hasDouble x)) (map show [start..end]))

getPart2 :: [String] -> Int
getPart2 = getPart2Amount . getRange
