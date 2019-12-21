module Day8
    ( getPart1
    , getPart2
    ) where

import Data.Function
import Data.List
import Data.List.Index
import Data.List.Split

getCount :: Int -> [Int] -> Int
getCount x = length . filter (x==)

getPart1 :: String -> Int
getPart1 str = (getCount 1 foundLayer) * (getCount 2 foundLayer)
  where
  numInputs = map (\c -> read [c]) str
  layers = chunksOf (25 * 6) numInputs
  foundLayer = minimumBy (compare `on` getCount 0) layers

getPixel :: Int -> Int -> Int
getPixel a b
  | a == 2 = b
  | otherwise = a

getPixelRow :: [Int] -> [Int] -> [Int]
getPixelRow acc next = imap (\index accItem -> getPixel accItem (next !! index)) acc

convertToColor:: [Int] -> [Char]
convertToColor xs = map (\x -> if (x == 0) then '-' else 'x') xs

showPixel 0 = " "
showPixel 1 = "\x2588"

getPart2 str = unlines $ chunksOf 25 image
  where
  numInputs = map (\c -> read [c]) str
  chunks = chunksOf (25 * 6) numInputs
  picture = map (head . dropWhile (==2)) $ transpose chunks
  image = concatMap showPixel picture
