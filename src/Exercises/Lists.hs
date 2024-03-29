module Exercises.Lists
  ( reverse'
  , count'
  , primes
  , fac'
  , power
  ) where

import           Data.Proxy
import           Sorting.Anamorphism  (Anamorphism (..), CoAlgebra (..))
import           Sorting.Catamorphism (Algebra (..), Catamorphism (..))
import           Sorting.Hylomorphism (Hylomorphism (..))

reverse' :: [a] -> [a]
reverse' = cata (Algebra [] (\x y -> y ++ [x]))

count' :: (Num a, Eq a, Ord a) => a -> [a]
count' = ana (CoAlgebra unfoldCount)

fac' :: Int -> Int
fac' = hylo (CoAlgebra unfoldCount, Algebra 1 (*))

power :: Int -> Int -> Int
power x = hylo (CoAlgebra unfoldCount, Algebra 1 (\_ v -> v * x))

primes :: [Int] -> [Int]
primes = ana (CoAlgebra unfoldPrimes)

unfoldCount :: (Num a, Eq a, Ord a) => a -> Either () (a, a)
unfoldCount n
  | n <= 0 = Left ()
  | otherwise = Right (n, n - 1)

unfoldPrimes :: [Int] -> Either () (Int, [Int])
unfoldPrimes []    = Left ()
unfoldPrimes (p:l) = Right (p, [x | x <- l, x `rem` p /= 0])
