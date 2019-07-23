module Data
  ( Tree(..)
  , Rose(..)
  , toTree
  , toBush
  , inorder
  , countOdd
  ) where

import           Control.Monad.State.Lazy

data Tree a
  = Empty
  | Node (Tree a)
         a
         (Tree a)
  deriving (Show, Eq)

data Rose a =
  Rose a
       [Rose a]

data Fork a
  = Leaf a
  | Fork (Fork a, Fork a)

data FunList a b c
  = Done a
         b
  | FunList a
            b
            (b -> c)

{-|
  toTree and toBush define an isomorphism
|-}
toTree :: [Rose a] -> Tree a
toTree []             = Empty
toTree (Rose a ts:us) = Node (toTree ts) a (toTree us)

toBush :: Tree a -> [Rose a]
toBush Empty               = []
toBush (Node left a right) = Rose a (toBush left) : toBush right

inorder :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
inorder _ Empty = pure Empty
inorder m (Node left a right) =
  pure Node <*> inorder m left <*> m a <*> inorder m right

countOdd :: Integer -> State Integer Bool
countOdd n
  | even n = pure False
countOdd _ = state (\n -> (True, n + 1))
