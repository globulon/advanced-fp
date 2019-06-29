module Category.Profunctor(Profunctor(..)) where

class Profunctor p where
  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d
  dimap f g = lmap f . rmap g
  lmap :: (a -> b) -> p b c -> p a c
  lmap f = dimap f id
  rmap :: (b -> d) -> p a b -> p a d
  rmap = dimap id
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}
  {-# MINIMAL dimap | (lmap, rmap)  #-}

instance Profunctor (->) where
  dimap f h g = h . g . f