{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sorting.Hylomorphism(Hylomorphism(..)) where

import Sorting.Catamorphism(Catamorphism(..), Algebra(..))
import Sorting.Anamorphism(Anamorphism(..), CoAlgebra(..))
import Data.Proxy

type Algebras x u v = (CoAlgebra x u, Algebra x v)

class Hylomorphism where
--  hylo :: (Catamorphism m, Anamorphism m) => proxy m -> CoAlgebra x u -> Algebra x v -> u -> v
  hylo :: Algebras x u v -> u -> v

instance Hylomorphism where
--  hylo = hylomorphism
  hylo (co, alg) u = case unfold co u of
    Left _       -> empty alg
    Right (x, u) -> fold alg x (hylo (co, alg) u)

{-|
  Mathematical version
  That version build for nothing the list and destroys it
  The optimised version just unfold the next output to fold up
  hylomorphism :: forall x u v proxy m.(Catamorphism m, Anamorphism m) => proxy m -> CoAlgebra x u -> Algebra x v -> u -> v
  hylomorphism _ c a = cata a . (ana :: CoAlgebra x u -> u -> m x) c
|-}
