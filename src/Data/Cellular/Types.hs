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

data U u c = U [u c] (u c) [u c]
  deriving (Show, Eq, Ord)

shiftUp, shiftDown :: U u c -> U u c
shiftUp   (U (a:as) x     bs) = U     as a (x:bs)
shiftDown (U     as x (b:bs)) = U (x:as) b     bs

instance (Functor u) => Functor (U u) where
  fmap f (U as x bs) = U (map (fmap f) as) 
                         (fmap f x) 
                         (map (fmap f) bs)

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

class Universe u where
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

  shift Up u = shiftUp u
  shift Down u = shiftDown u
  shift d (U as x bs) = let s = shift (demote d)
                        in U (map s as)
                             (s x)
                             (map s bs)

----------------------------------------------------------------------
---- Universes and Cells ---------------------------------------------
----------------------------------------------------------------------

-- class Cell u c | c -> u where
--   stepCell :: u c -> c


-- class ToChar c where
--   toChar :: c -> Char
