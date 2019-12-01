module Day1
    ( getResult
    , getRealMass
    ) where

getMass :: Int -> Int
getMass x = (floor (realToFrac (x `div` 3)) - 2)

getRealMass :: Int -> Int -> Int
getRealMass acc x =
  let nextMass = getMass x
  in
    if (nextMass < 1) then acc
    else getRealMass (acc + nextMass) nextMass

getResult :: [String] -> Int
getResult = sum . map ((getRealMass 0) . read)

