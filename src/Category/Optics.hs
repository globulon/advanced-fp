{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types     #-}

module Category.Optics
  ( Lens(..)
  , Prism(..)
  , Adapter(..)
  , AdapterP(..)
  , flatten
  , fstL
  , sndL
  , sign
  ) where

import           Category.Profunctor (Cartesian (..), CoCartesian (..),
                                      Monoidal (..), Profunctor (..), cross)

data Lens a b s t = Lens
  { view   :: s -> a
  , update :: (b, s) -> t
  }

data Prism a b s t = Prism
  { match :: s -> Either t a
  , build :: b -> t
  }

data Adapter a b s t = Adapter
  { from :: s -> a
  , to   :: b -> t
  }

instance Profunctor (Lens a b) where
  {-|
    (s' -> s) -> (t -> t') -> Lens a b s t -> Lens a b s' t'
    (s' -> s) -> (t -> t') -> (Lens s -> a (b,s) -> t) -> (Lens s' -> a (b,s') -> t')
  |-}
  dimap f g (Lens vw upd) = Lens (vw . f) (g . upd . cross id f)
  {-# INLINE dimap #-}


instance Cartesian (Lens a b) where
  {-|
  (b,s) -> t    (b,(s, c)) -> (t, c)
    (Lens s -> a (b,s) -> t) -> (Lens (s, c) -> a (b,(s, c)) -> (t, c))
  |-}
  first (Lens vw upd) = Lens (vw . fst) (fork (upd . cross id fst) (snd . snd))
  {-# INLINE first #-}
  {-|
    (b,s) -> t    (b,(c, s)) -> (c, t)
    (Lens s -> a (b,s) -> t) -> (Lens (c, s) -> a  (b,(c, s)) -> (c, t))
  |-}
  second (Lens vw upd) = Lens (vw . snd) (fork (fst . snd) (upd . cross id snd))
  {-# INLINE second #-}


instance Profunctor (Adapter a b) where
  {-| (s' -> s) -> (t -> t') -> Adapter a b s t -> Adapter a b s' t' |-}
  dimap f g (Adapter from to) = Adapter (from . f) (g . to)

{-|
  Recall the concrete representation of adapters
  data Adapter a b s t = Adapter {from :: s → a,to :: b → t}

  The two methods from and to of an Adapter A B S T do not generally compose,
  specifically when types A and B differ. However, if we could somehow transform As
  into Bs, then we could make the two methods fit together; and moreover, we would
  then be able to transform Ss into Ts in the same way.

  Which is to say, there is an
  obvious mapping that takes an Adapter A B S T and a P A B and yields a P S T, provided
  that P is a profunctor. This motivates the following datatype definitions
|-}
type Optics p a b s t = p a b -> p s t

type AdapterP a b s t
   = forall p. Profunctor p =>
                 Optics p a b s t

type LensP a b s t
   = forall p. Cartesian p =>
                 Optics p a b s t

type PrismP a b s t
   = forall p. CoCartesian p =>
                 Optics p a b s t

adapterC2P :: Adapter a b s t -> AdapterP a b s t
adapterC2P (Adapter o i) = dimap o i

adapterP2C :: AdapterP a b s t -> Adapter a b s t
adapterP2C l = l (Adapter id id)

lensC2P :: Lens a b s t -> LensP a b s t
lensC2P (Lens vw upd) = dimap (fork vw id) upd . first

lensP2C :: LensP a b s t -> Lens a b s t
lensP2C l = l (Lens id fst)
{-|
  Applies 2 functions to an argument to build a pair
|-}
fork :: (a -> b) -> (a -> c) -> a -> (b, c)
fork f g x = (f x, g x)

fstL :: Lens a b (a, c) (b, c)
fstL = Lens vw updt
  where
    vw = fst
    updt (z, (_, y)) = (z, y)

sndL :: Lens a b (c, a) (c, b)
sndL = Lens vw up
  where
    vw = snd
    up (z, (x, _)) = (x, z)

sign :: Lens Bool Bool Integer Integer
sign = Lens vw upd
  where
    vw x = x >= 0
    upd (True, x) = abs x
    upd (_, x)    = -upd (True, x)

the :: Prism a b (Maybe a) (Maybe b)
the = Prism mt bd
  where
    mt (Just a) = Right a
    mt _        = Left Nothing
    bd = Just

whole :: Prism Integer Integer Double Double
whole = Prism match build
  where
    match x
      | f == 0 = Right n
      | otherwise = Left x
      where
        (n, f) = properFraction x
    build = fromIntegral

flatten :: Adapter (a, b, c) (a', b', c') ((a, b), c) ((a', b'), c')
flatten = Adapter from to
  where
    from ((a, b), c) = (a, b, c)
    to (a, b, c) = ((a, b), c)

flattenP :: AdapterP (a, b, c) (a', b', c') ((a, b), c) ((a', b'), c')
flattenP = dimap from to
  where
    from ((a, b), c) = (a, b, c)
    to (a, b, c) = ((a, b), c)
