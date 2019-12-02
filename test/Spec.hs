import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.List.Split as Split

import Day1
import Day2

main :: IO ()
main = do
  defaultMain (testGroup "AOC 2019 Tests" [day1Test, day2Test, day2InitTest])

day1Test :: TestTree
day1Test = testCase "Testing getResult now"
  (assertEqual "Should calculate the result" 966 (getRealMass 1969))

day2Test :: TestTree
day2Test = testCase "Testing runProgram"
  (assertEqual "Should calculate the result" 30 (runProgram 0 [1,1,1,4,99,5,6,0,99]))

day2InitTest :: TestTree
day2InitTest = testCase "Testing initialize"
  (assertEqual "Should calculate the result" [1, 12, 2] (initializeProgram (map read (Split.splitOn "," "1,1,1"))))
