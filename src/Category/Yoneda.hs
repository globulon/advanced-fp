{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types     #-}

module Category.Yoneda where

{-|
  Laws
  fw . bw = id
  bw . fw = id
|-}
fw :: (Functor f) => (forall b. (a -> b) -> f b) -> f a
fw f = f id

bw :: (Functor f) => f a -> (forall b. (a -> b) -> f b)
bw x f = fmap f x
