{-# LANGUAGE TupleSections #-}

module Category.Profunctor
  ( Profunctor(..)
  , Cartesian(..)
  , CoCartesian(..)
  ) where

import Control.Arrow

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
  {-# MINIMAL dimap | (lmap, rmap) #-}

{-|
  Whenever you have a functor you can lift it
  into a profunctor by reifying an abstraction
  a -> f b
  We chose here a Kleisli. That might be an improper
  choice. We could force ourselves to redefine a new datatype again
|-}
instance (Functor f) => Profunctor (Kleisli f) where
  dimap f g (Kleisli h) = Kleisli (fmap g . h . f)

instance Profunctor (->) where
  dimap f h g = h . g . f
  {-# INLINE dimap #-}

class Profunctor p => Cartesian p where
  first :: p a b -> p (a, c) (b, c)
  second :: p a b -> p (c, a) (c, b)
  {-# MINIMAL first, second #-}

instance Cartesian (->) where
  first h = cross h id
  second  = cross id
  {-# INLINE first #-}
  {-# INLINE second #-}

instance Functor f => Cartesian (Kleisli f) where
  first  k = Kleisli ( rstrength . cross (runKleisli k) id )
  second k = Kleisli ( lstrength . cross id (runKleisli k) )
  {-# INLINE first #-}
  {-# INLINE second #-}

rstrength :: (Functor f) => (f a, b) -> f (a , b)
rstrength (fa, y) = fmap (,y) fa

lstrength :: (Functor f) => (a, f b) -> f (a , b)
lstrength (x, fb) = fmap (x,) fb



class Profunctor p =>  CoCartesian p where
  left :: p a b -> p (Either a c) (Either b c)
  right :: p a b -> p (Either c a) (Either c b)
  {-# MINIMAL left, right #-}

instance CoCartesian (->) where
  left  h = plus h id
  right h = plus id h
  {-# INLINE left #-}
  {-# INLINE right #-}

instance Applicative f => CoCartesian (Kleisli f) where
  left  (Kleisli k) = Kleisli  ( either (fmap Left . k) (pure . Right) )
  right (Kleisli k) = Kleisli (either  (pure . Left) (fmap Right . k))
  {-# INLINE left #-}
  {-# INLINE right #-}

class Profunctor p => Monoidal p where
  par :: p a b -> p c d -> p (a, c) (b, d)
  pempty :: p () ()
  {-# MINIMAL par, pempty #-}

instance Monoidal (->) where
  par = cross
  pempty = id
  {-# INLINE par #-}
  {-# INLINE pempty #-}

instance Applicative f => Monoidal (Kleisli f) where
  par (Kleisli h) (Kleisli k) = Kleisli (pair h k)
  pempty                      = Kleisli pure

pair :: Applicative f => (a -> f b) -> (c -> f d) -> (a, c) -> f (b, d)
pair h k (x, y) = pure (,) <*> h x <*> k y

plus :: (a -> c) -> (b -> d) -> Either a b -> Either c d
plus f g (Left x) = Left (f x)
plus _ g (Right y) = Right (g y)

assoc :: ((a, b), c) -> (a, (b, c))
assoc ((a, b), c) = (a, (b, c))

assoc' :: (a, (b, c)) -> ((a, b), c)
assoc' (a, (b, c)) = ((a, b), c)

lunit :: ((), a) -> a
lunit (_, a) = a

lunit' :: a -> ((), a)
lunit' x = ((), x)

cross :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
cross f g (x, y) = (f x, g y)
