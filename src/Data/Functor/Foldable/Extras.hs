{-# LANGUAGE FlexibleContexts #-}

module Data.Functor.Foldable.Extras where

import Data.Functor.Foldable
import Control.Monad ((<=<))

-- | A monadic catamorphism.
cataM
  :: (Recursive t, Traversable (Base t), Monad m)
  => (Base t a -> m a) -- ^ a monadic (Base t)-algebra
  -> t                 -- ^ fixed point
  -> m a               -- ^ result
cataM f = c where c = f <=< (traverse c . project)

-- | A monadic anamorphism
anaM
  :: (Corecursive t, Traversable (Base t), Monad m)
  => (a -> m (Base t a))        -- ^ a monadic (Base t)-coalgebra
  -> a                          -- ^ seed
  -> m t
anaM g = a where a = fmap embed . traverse a <=< g

-- | A catamorphism which works on two structures at once.
-- it can be used to encode "zip-like" functions. For instance:
--
-- > zip :: [a] -> [b] -> [(a,b)]
-- > zip = zipo alg where
-- >   alg Nil _ = []
-- >   alg _ Nil = []
-- >   alg (Cons x xs) (Cons y ys) = (x,y) : xs ys
--
zipo :: (Recursive g, Recursive h)
     => (Base g (h -> c) -> Base h h -> c) -- ^ An algebra for two Recursives
     -> g                                  -- ^ first fixed point
     -> h                                  -- ^ second
     -> c                                  -- ^ result
zipo alg = cata zalg where zalg x = alg x . project
