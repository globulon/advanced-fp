module Data.FunList where

data FunList a b t = Done t | More a (FunList a b (b -> t))
newtype Traversal a b s t = Traversal { extract :: s -> FunList a b t }
{-|
  Qualifies the isomorphisms FunList a b t and t + (a x FunList a b (b -> t))
|-}

out :: FunList a b t -> Either t (a, FunList a b (b -> t))
out (Done z)   = Left z
out (More x l) = Right (x, l)

inn :: Either t (a, FunList a b (b -> t)) -> FunList a b t
inn (Left t) = Done t
inn (Right (x, l)) = More x l

single :: a -> FunList a b b
single x = More x (Done id)

fuse :: FunList b b t -> t
fuse (Done z) = z
-- l :: FunList b b (b -> t)
fuse (More x l) = fuse l x

instance Functor (FunList a b) where
  fmap f (Done x) = Done (f x)
  fmap f (More x l) = More x (fmap (f.) l)

instance Applicative (FunList a b) where
  pure = Done
  Done f <*> val   = fmap f val
  {-|
    FunList a b (b -> t -> u) <*> FunList a b t
    flip :: (a -> b -> c) -> b -> a -> c
  -}
  More x l <*> val = More x (fmap flip l <*> val)
