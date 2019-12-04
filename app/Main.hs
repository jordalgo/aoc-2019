module Main where

import Lib
import Day3

main :: IO ()
main = do
    contents <- readFile "src/input/day3.txt"
    putStrLn (show (getResult (lines contents)))
