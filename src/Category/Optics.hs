module Category.Optics where

data Lens a b s t = Lens { view :: s -> a , update :: (s, b) -> t }

data Prism a b s t = Prism { match :: s -> Either t a, build :: b  -> t }

data Adapter a b s t = Adapter { from :: s -> a, to :: b -> t }

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

nested :: Adapter (a, b, c) (a', b', c') ((a, b), c) ((a', b'), c')
nested = Adapter from to where
  from ((a, b), c) = (a, b, c)
  to (a, b, c) = ((a, b), c)