{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sorting.Hylomorphism(Hylomorphism(..)) where

import Sorting.Catamorphism(Catamorphism(..), Algebra(..))
import Sorting.Anamorphism(Anamorphism(..), CoAlgebra(..))
import Data.Proxy

class Hylomorphism where
  hylo :: (Catamorphism m, Anamorphism m) => proxy m -> CoAlgebra x u -> Algebra x v -> u -> v

instance Hylomorphism where
  hylo = hylomorphism


hylomorphism :: forall x u v proxy m.(Catamorphism m, Anamorphism m) => proxy m -> CoAlgebra x u -> Algebra x v -> u -> v
hylomorphism _ c a = cata a . (ana :: CoAlgebra x u -> u -> m x) c
