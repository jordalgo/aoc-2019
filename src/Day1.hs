module Day1
    ( getTotalMass
    , getTotalMassWithFuel
    , getRealMass
    ) where

getMass :: Int -> Int
getMass = (+ (-2)) . (`div` 3)

getRealMass :: Int -> Int
getRealMass =
  go 0 . getMass
  where go acc y = if (y < 1) then acc else go (acc + y) (getMass y)

getTotalMass :: [String] -> Int
getTotalMass = sum . map (getMass . read)

getTotalMassWithFuel :: [String] -> Int
getTotalMassWithFuel = sum . map (getRealMass . read)

