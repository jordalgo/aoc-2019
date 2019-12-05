module Day4
    ( getPart1
    ) where

digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

areDigitsAsc :: [Int] -> Bool
areDigitsAsc [] = True
areDigitsAsc (x:[]) = True
areDigitsAsc (x:xs) = if (next < x) then False else areDigitsAsc xs
  where next = head xs

hasSameAdjacentDigits :: [Int] -> Bool
hasSameAdjacentDigits [] = False
hasSameAdjacentDigits (x:[]) = False
hasSameAdjacentDigits (x:xs) = if (next == x) then True else hasSameAdjacentDigits xs
  where next = head xs

getPart1 input = length (filter (\x -> (areDigitsAsc x) && (hasSameAdjacentDigits x)) (map digits inputRange))
  where
  nums = map read input
  inputRange = [(nums !! 0)..(nums !! 1)]
