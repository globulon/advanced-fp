module Exercises.Lists(reverse', count') where

import Sorting.Catamorphism(Algebra(..), Catamorphism(..))
import Sorting.Anamorphism(Anamorphism(..), CoAlgebra(..))

reverse' :: [a] -> [a]
reverse' = cata (Algebra [] (\x y -> y ++ [x]))

count' :: (Num a, Eq a, Ord a) => a -> [a]
count'  = ana (CoAlgebra unfoldCount)

unfoldCount :: (Num a, Eq a, Ord a) => a -> Either () (a, a)
unfoldCount n
  | n <= 0    = Left ()
  | otherwise = Right (n, n - 1)