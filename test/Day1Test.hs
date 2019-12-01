module Day1Test where

import Test.Tasty
import Test.Tasty.HUnit

import Day1

getResultTest :: TestTree
getResultTest = testCase "Testing getResult now"
  (assertEqual "Should calculate the result" 10 (getResult ["+1", "-1", "+5", "+5"]))

getRepeatResultTest :: TestTree
getRepeatResultTest = testCase "Testing getRepeatResult now"
  (assertEqual "Should calculate the first repeat" 10 (getRepeatsResult ["+3", "+3", "+4", "-2", "-4"]))

firstDupTest :: TestTree
firstDupTest = testCase "Testing part2 now"
  (assertEqual "Should calculate the first repeat" (Just 10) (part2 ["+3", "+3", "+4", "-2", "-4"]))

