module Category.Optics where

data Lens a b s t = Lens { view :: s -> a , update :: (s, b) -> t }

sndLens :: Lens a b (c, a) (c, b)
sndLens = Lens vw up where
  vw             = snd
  up ((x, y), z) = (x, z)

