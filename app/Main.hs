module Main where

import Lib
import Day1

main :: IO ()
main = do
    fileLines <- getFileLines "src/input/day1.txt"
    let eachl = fileLines
    putStrLn (show (getTotalMass eachl))
