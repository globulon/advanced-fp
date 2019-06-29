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

class Profunctor p => Cartesian p where
  second :: p a b -> p (c, a) (c, b)
  {-# MINIMAL second #-}

class Profunctor p => CoCartesian p where
  right :: p a b -> p (Either c a) (Either c b)
  {-# MINIMAL right #-}

instance Profunctor (->) where
  dimap f h g = h . g . f
  {-# INLINE dimap #-}

instance Cartesian (->) where
  second = fmap
  {-# INLINE second #-}

instance CoCartesian (->) where
  right = fmap
  {-# INLINE right #-}
