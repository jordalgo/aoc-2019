module Main where

import Lib
import Day2

main :: IO ()
main = do
    contents <- readFile "src/input/day2.txt"
    putStrLn (show (getResult contents))
