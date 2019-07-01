module Data(Rose(..)) where

data Tree a = Root | Node (Tree a, a ,Tree a)
data Rose a = Rose a [Rose a]
data Fork a = Leaf a | Fork (Fork a, Fork a)

{-|
  toTree and toBush define an isomorphism
|-}
toTree :: [Rose a] -> Tree a
toTree []               = Root
toTree (Rose a ts : us) = Node (toTree ts, a, toTree us)

toBush :: Tree a -> [Rose a]
toBush Root                    = []
toBush (Node (left, a, right)) = Rose a (toBush left) : toBush right
