module Main where

import Lib
import Day1
import Day2

main :: IO ()
main = do
    putStrLn "Hello, whats your file?"
    name <- getLine
    fileLines <- getFileLines name
    let eachl = fileLines
    putStrLn (show (getCheckSum eachl))
