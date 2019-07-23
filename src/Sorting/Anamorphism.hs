module Sorting.Anamorphism
  ( CoAlgebra(..)
  , Anamorphism(..)
  ) where

newtype CoAlgebra x u = CoAlgebra
  { unfold :: u -> Either () (x, u)
  }

class Anamorphism m where
  ana :: CoAlgebra x u -> u -> m x
  {-# MINIMAL ana #-}

instance Anamorphism [] where
  ana co u =
    case unfold co u of
      Left _       -> []
      Right (x, r) -> x : ana co r
