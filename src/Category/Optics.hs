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

import Category.Profunctor(Profunctor(..))

data Lens a b s t = Lens { view :: s -> a , update :: (b, s) -> t }

data Prism a b s t = Prism { match :: s -> Either t a, build :: b  -> t }

data Adapter a b s t = Adapter { from :: s -> a, to :: b -> t }

type AdapterP a b s t = forall p. Profunctor p => p a b -> p s t

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