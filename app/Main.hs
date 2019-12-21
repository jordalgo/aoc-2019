module Main where

import Lib
import Day8

main :: IO ()
main = do
    contents <- readFile "src/input/day8.txt"
    putStrLn (show (getPart1 contents))
    putStrLn (getPart2 contents)
  --putStrLn (show (getPart1 (lines contents)))
    --putStrLn (show (getPart2 (lines contents)))
