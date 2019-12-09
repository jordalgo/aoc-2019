module Main where

import Lib
import Day5

main :: IO ()
main = do
    contents <- readFile "src/input/day5.txt"
    putStrLn (show (getPart1 contents))
    --putStrLn (show (getPart2 (lines contents)))
