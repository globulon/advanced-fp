module Sorting.Catamorphism(Algebra(..), Catamorphism(..)) where

data Algebra t u = Algebra {
  empty :: u,
  fold :: t -> u -> u
}

class Catamorphism m where
  cata :: Algebra x u -> m x -> u
  {-# MINIMAL cata #-}

instance Catamorphism [] where
  cata alg []    = empty alg
  cata alg (h:t) = (fold alg)  h (cata alg t)
