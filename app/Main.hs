module Main where

import Lib
import Day9

main :: IO ()
main = do
    contents <- readFile "src/input/day9.txt"
    getPart1 contents
    --putStrLn (show (getPart2 contents))
  --putStrLn (show (getPart1 (lines contents)))
    --putStrLn (show (getPart2 (lines contents)))
