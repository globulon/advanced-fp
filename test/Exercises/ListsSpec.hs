module Exercises.ListsSpec(spec) where

import Exercises.Lists(reverse', count', primes)
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

spec_prime :: Spec
spec_prime = describe "prime should" $ do
  it "give expected series" $
    take 5 (primes [2..]) `shouldBe` [2,3,5,7,11]
  it "give empty series" $
    primes [] `shouldBe` []
  it "give series in  boundaries" $
    (take 10 . primes $ [2..20]) `shouldBe` [2,3,5,7,11,13,17,19]


checkReverse :: Int -> Bool
checkReverse n = count' n == reverse' [1 .. n]