module Day4
    ( getPart1
    ) where

import Data.List

areDigitsAsc :: String -> Bool
areDigitsAsc xs = sort xs == xs

hasSameAdjacentDigits :: String -> Bool
hasSameAdjacentDigits = any (>1) . map length . group

getPart1Amount :: (Int, Int) -> Int
getPart1Amount (start, end) = length (filter (\x -> (areDigitsAsc x) && (hasSameAdjacentDigits x)) (map show [start..end]))

getPart1 :: [String] -> Int
getPart1 input = getPart1Amount ((nums !! 0), (nums !! 1))
  where
  nums = map read input
