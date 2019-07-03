{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types  #-}
module Category.Optics(
  Lens(..),
  Prism(..),
  Adapter(..),
  AdapterP(..),
  flatten) where

import Category.Profunctor(Profunctor(..))

data Lens a b s t = Lens { view :: s -> a , update :: (s, b) -> t }

data Prism a b s t = Prism { match :: s -> Either t a, build :: b  -> t }

data Adapter a b s t = Adapter { from :: s -> a, to :: b -> t }

type AdapterP a b s t = forall p. Profunctor p => p a b -> p s t

sndLens :: Lens a b (c, a) (c, b)
sndLens = Lens vw up where
  vw             = snd
  up ((x, y), z) = (x, z)

the :: Prism a b (Maybe a) (Maybe b)
the = Prism mt bd
  where
    mt (Just a) = Right a
    mt _ = Left Nothing
    bd = Just

flatten :: Adapter (a, b, c) (a', b', c') ((a, b), c) ((a', b'), c')
flatten = Adapter from to where
  from ((a, b), c) = (a, b, c)
  to (a, b, c) = ((a, b), c)

flattenP :: AdapterP (a, b, c) (a', b', c') ((a, b), c) ((a', b'), c')
flattenP = dimap from to
  where
    from ((a, b), c) = (a, b, c)
    to (a, b, c) = ((a, b), c)