import Test.Tasty
import Test.Tasty.HUnit

import Day1

main :: IO ()
main = do
  defaultMain (testGroup "AOC 2019 Tests" [day1Test])

day1Test :: TestTree
day1Test = testCase "Testing getResult now"
  (assertEqual "Should calculate the result" 966 (getRealMass 1969))

