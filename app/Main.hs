module Main where

import Lib
import Day1

main :: IO ()
main = do
    putStrLn "Hello, whats your file?"
    name <- getLine
    let fileName = name
    fileLines <- getFileLines ("src/input/" ++ fileName)
    let eachl = fileLines
    putStrLn (show (getResult eachl))
