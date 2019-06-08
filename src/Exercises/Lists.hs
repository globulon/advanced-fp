module Exercises.Lists(reverse') where

import Sorting.Catamorphism(Algebra(..), Catamorphism(..))

reverse' :: [a] -> [a]
reverse' = cata (Algebra [] (\x y -> y ++ [x]))
