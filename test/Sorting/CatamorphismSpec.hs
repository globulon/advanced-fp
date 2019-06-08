module Sorting.CatamorphismSpec where

import Sorting.Catamorphism(Catamorphism(..), Algebra(..))
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "list cata" $ do
  it "should fold product on empty list" $
    cata (Algebra 1 (*)) [] `shouldBe` 1
  it "should fold product on non empty list" $
    property (\n -> cata (Algebra (1::Int) (*)) [1..n] == product [1..n])
  it "should fold sum on empty list" $
    cata (Algebra 0 (+)) [] `shouldBe` 0
  it "should fold sum on non empty list" $
    property (\n -> cata (Algebra (0::Int) (+)) [1..n] == sum [1..n])
