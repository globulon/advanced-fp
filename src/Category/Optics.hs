{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types  #-}
module Category.Optics(
  Lens(..),
  Prism(..),
  Adapter(..),
  AdapterP(..),
  flatten,
  fstL, sndL,
  sign) where

import Category.Profunctor(Profunctor(..), Cartesian(..), CoCartesian(..), Monoidal(..), cross)

data Lens a b s t = Lens { view :: s -> a , update :: (b, s) -> t }

data Prism a b s t = Prism { match :: s -> Either t a, build :: b  -> t }

data Adapter a b s t = Adapter { from :: s -> a, to :: b -> t }

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

type AdapterP a b s t = forall p. Profunctor p => Optics p a b s t

type LensP a b s t = forall p. Cartesian p => Optics p a b s t

type PrismP a b s t = forall p. CoCartesian p => Optics p a b s t

adapterC2P :: Adapter a b s t -> AdapterP a b s t
adapterC2P (Adapter o i) = dimap o i

instance Profunctor (Lens a b) where
  -- (s' -> s) -> (t -> t') -> Lens a b s t -> Lens a b s' t'
  -- (s' -> s) -> (t -> t') -> (Lens s -> a (b,s) -> t) -> (Lens s' -> a (b,s') -> t')
  dimap f g (Lens vw upd) = Lens (vw . f) (g . upd . cross id f)
  {-# INLINE dimap #-}

instance Cartesian (Lens a b) where
--  first :: p s t -> p (s, q) (t, q)
--  first :: Lens (s -> a (b, s) -> t) -> Lens((s,q) -> a (b, (s, q)) -> (t, q))
  first (Lens vw upd) = undefined
--  second :: p a b -> p (c, a) (c, b)
  second = undefined

fork :: (a -> b) -> (a -> c) -> a -> (b, c)
fork f g x = (f x, g x)

fstL :: Lens a b (a, c) (b, c)
fstL = Lens vw updt where
  vw = fst
  updt (z, (_, y)) = (z, y)

sndL :: Lens a b (c, a) (c, b)
sndL = Lens vw up where
  vw             = snd
  up (z, (x, _)) = (x, z)

sign :: Lens Bool Bool Integer Integer
sign = Lens vw upd where
  vw x = x >= 0
  upd (True, x) = abs x
  upd (_,    x) = - upd (True, x)

the :: Prism a b (Maybe a) (Maybe b)
the = Prism mt bd
  where
    mt (Just a) = Right a
    mt _ = Left Nothing
    bd = Just

whole :: Prism Integer Integer Double Double
whole = Prism match build where
  match x
    | f == 0    = Right n
    | otherwise = Left x
    where (n, f) = properFraction x
  build = fromIntegral

flatten :: Adapter (a, b, c) (a', b', c') ((a, b), c) ((a', b'), c')
flatten = Adapter from to where
  from ((a, b), c) = (a, b, c)
  to (a, b, c) = ((a, b), c)

flattenP :: AdapterP (a, b, c) (a', b', c') ((a, b), c) ((a', b'), c')
flattenP = dimap from to
  where
    from ((a, b), c) = (a, b, c)
    to (a, b, c) = ((a, b), c)