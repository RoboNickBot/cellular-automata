{-# LANGUAGE GADTs #-}

module Data.Cellular.Types where

import Control.Comonad

----------------------------------------------------------------------

data C c = C c
  deriving (Show, Eq, Ord)

instance Functor C where
  fmap f (C c) = C (f c)

instance Comonad C where
  extract (C x) = x
  duplicate c = C c

----------------------------------------------------------------------

data U u c = U [u c] (u c) [u c]
  deriving (Show, Eq, Ord)

umap :: (u c -> v k) -> U u c -> U v k
umap f (U as x bs) = U (map f as) (f x) (map f bs)

shiftUp, shiftDown :: U u c -> U u c
shiftUp   (U (a:as) x     bs) = U     as a (x:bs)
shiftDown (U     as x (b:bs)) = U (x:as) b     bs

instance (Functor u) => Functor (U u) where                       
  fmap = umap . fmap

instance (Comonad u) => Comonad (U u) where
  extract (U _ x _) = extract x
  duplicate u = U (dshift shiftUp u)
                  (dupSlice u)
                  (dshift shiftDown u)

dshift s = map dupSlice . tail . iterate s

dupSlice :: (Comonad u) => U u c -> u (U u c)
dupSlice u@(U _ x _) = fmap (const u) x

----------------------------------------------------------------------

data D u where
  Base ::        D C
  D    :: D u -> D (U u)
  Up   ::        D u
  Down ::        D u

class Comonad u => Universe u where
  empty :: D u
  demote :: D (U u) -> D u
  shift :: D u -> u c -> u c

instance Universe C where
  empty = Base
  demote _ = Base
  shift _ = id

instance (Universe u) => Universe (U u) where
  empty = D empty
  
  demote (D d) = d
  demote _ = empty

  shift Up   = shiftUp
  shift Down = shiftDown
  shift d    = umap (shift (demote d))

get :: (Universe u) => D u -> u c -> c
get d = extract . shift d

----------------------------------------------------------------------
---- Universes and Cells ---------------------------------------------
----------------------------------------------------------------------

-- class Cell u c | c -> u where
--   stepCell :: u c -> c


-- class ToChar c where
--   toChar :: c -> Char
