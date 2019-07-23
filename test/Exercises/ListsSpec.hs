module Exercises.ListsSpec
  ( spec
  ) where

import           Exercises.Lists (count', fac', power, primes, reverse')
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  spec_reverse
  spec_count
  spec_fac
  spec_prime
  spec_pow

spec_reverse :: Spec
spec_reverse =
  describe "reverse'" $ do
    it "should conserve empty list" $ (reverse' [] :: [Int]) `shouldBe` []
    it "should reverse empty list" $
      property (\xs -> (reverse' xs :: [Int]) == reverse xs)

spec_count :: Spec
spec_count =
  describe "count should" $ do
    it "conserve 0" $ count' 0 `shouldBe` []
    it "generate reverse list" $ property checkReverse

spec_prime :: Spec
spec_prime =
  describe "prime should" $ do
    it "give expected series" $
      take 5 (primes [2 ..]) `shouldBe` [2, 3, 5, 7, 11]
    it "give empty series" $ primes [] `shouldBe` []
    it "give series in  boundaries" $
      (take 10 . primes $ [2 .. 20]) `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19]

spec_fac :: Spec
spec_fac =
  describe "factorial" $ do
    it "should find 5!" $ fac' 5 `shouldBe` 120
    it "should find 1" $ fac' 1 `shouldBe` 1

spec_pow :: Spec
spec_pow =
  describe "power" $ do
    it "should process power 2 4" $ power 2 4 `shouldBe` 16
    it "should process power x 0" $ property (\x -> power x 0 == 1)

checkReverse :: Int -> Bool
checkReverse n = count' n == reverse' [1 .. n]
