module Main where

import Lib
import Day4

main :: IO ()
main = do
    contents <- readFile "src/input/day4.txt"
    putStrLn (show (getPart1 (lines contents)))
    putStrLn (show (getPart2 (lines contents)))
