import Test.Tasty
import Test.Tasty.HUnit

import Day1

main :: IO ()
main = do
  defaultMain (testGroup "Our Library Tests" [getResultTest])

getResultTest :: TestTree
getResultTest = testCase "Testing getResult now"
  (assertEqual "Should calculate the result" 10 (getResult ["+1", "-1", "+5", "+5"]))

