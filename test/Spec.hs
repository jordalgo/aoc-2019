import Test.Tasty
import Test.Tasty.HUnit

import Day1Test

main :: IO ()
main = do
  defaultMain (testGroup "Our Library Tests" [getResultTest, getRepeatResultTest, firstDupTest])
