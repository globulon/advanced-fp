module Exercises.ListsSpec(spec) where

import Exercises.Lists(reverse')
import Test.Hspec
import Test.QuickCheck


spec :: Spec
spec = describe "reverse'" $ do
  it "should conserve empty list" $
    (reverse' []::[Int]) `shouldBe` []
  it "should reverse empty list" $
    property (\xs -> (reverse' xs::[Int]) == reverse xs)
