module Lib
    ( getFileLines
    ) where

getFileLines fileName = do
    contents <- readFile fileName
    let fileLines = lines contents
    return fileLines
