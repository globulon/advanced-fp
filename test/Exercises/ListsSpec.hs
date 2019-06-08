module Exercises.ListsSpec(spec) where

import Exercises.Lists(reverse', count')
import Test.Hspec
import Test.QuickCheck


spec :: Spec
spec = do
  spec_reverse
  spec_count

spec_reverse :: Spec
spec_reverse = describe "reverse'" $ do
  it "should conserve empty list" $
    (reverse' []::[Int]) `shouldBe` []
  it "should reverse empty list" $
    property (\xs -> (reverse' xs::[Int]) == reverse xs)

spec_count :: Spec
spec_count = describe "count should" $ do
  it "conserve 0" $
    count' 0 `shouldBe` []
  it "generate reverse list" $
    property checkReverse

checkReverse :: Int -> Bool
checkReverse n = count' n == reverse' [1 .. n]