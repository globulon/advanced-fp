{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Sorting.Hylomorphism
  ( Hylomorphism(..)
  ) where

import           Data.Proxy
import           Sorting.Anamorphism  (Anamorphism (..), CoAlgebra (..))
import           Sorting.Catamorphism (Algebra (..), Catamorphism (..))

type Algebras x u v = (CoAlgebra x u, Algebra x v)

class Hylomorphism where
  hylo :: Algebras x u v -> u -> v

--  hylo :: (Catamorphism m, Anamorphism m) => proxy m -> CoAlgebra x u -> Algebra x v -> u -> v
instance Hylomorphism
--  hylo = hylomorphism
                        where
  hylo (co, alg) u =
    case unfold co u of
      Left _       -> empty alg
      Right (x, u) -> fold alg x (hylo (co, alg) u)
{-|
  Mathematical version
  That version build for nothing the list and destroys it
  The optimised version just unfold the next output to fold up
  hylomorphism :: forall x u v proxy m.(Catamorphism m, Anamorphism m) => proxy m -> CoAlgebra x u -> Algebra x v -> u -> v
  hylomorphism _ c a = cata a . (ana :: CoAlgebra x u -> u -> m x) c
|-}
