module Day1
    ( getResult
    , getRealMass
    ) where

getMass :: Int -> Int
getMass x = (floor (realToFrac (x `div` 3)) - 2)

getRealMass :: Int -> Int
getRealMass x =
  go 0 (getMass x)
  where go acc y = if (y < 1) then acc else go (acc + y) (getMass y)

getResult :: [String] -> Int
getResult = sum . map (getRealMass . read)

