module Main where

import Lib
import Day6

main :: IO ()
main = do
    contents <- readFile "src/input/day6.txt"
    putStrLn (show (getPart1 (lines contents)))
    putStrLn (show (getPart2 (lines contents)))
