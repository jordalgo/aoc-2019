module Main where

import Lib
import Day7

main :: IO ()
main = do
    contents <- readFile "src/input/day7.txt"
    putStrLn (show (getPart1 contents))
    putStrLn (show (getPart2 contents))
  --putStrLn (show (getPart1 (lines contents)))
    --putStrLn (show (getPart2 (lines contents)))
