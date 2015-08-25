{-# LANGUAGE GADTs #-}

module Data.Cellular.Types where

import Control.Comonad
import Control.Applicative ((<$))

----------------------------------------------------------------------

-- an alternate form for U, using GADTs
data U' u c where
  C' :: c -> U' C c
  U' :: [u c] -> (u c) -> [u c] -> U' u c

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

umap :: (u c -> v k) -> U u c -> U v k
umap f (U as x bs) = U (map f as) (f x) (map f bs)

promote :: U u c -> U (U u) c
promote u = let ds f = tail . iterate f
            in U (ds shiftUp u) u (ds shiftDown u)

demote :: U u c -> u c
demote (U _ x _) = x

instance (Functor u) => Functor (U u) where                       
  fmap = umap . fmap

instance (Comonad u) => Comonad (U u) where
  extract (U _ x _) = extract x
  duplicate = umap (\u -> u <$ demote u) . promote

----------------------------------------------------------------------

data D u where
  Base ::        D C
  D    :: D u -> D (U u)
  Up   ::        D u
  Down ::        D u

class Comonad u => Universe u where
  empty :: D u
  demoteD :: D (U u) -> D u
  shift :: D u -> u c -> u c

instance Universe C where
  empty = Base
  demoteD _ = Base
  shift _ = id

instance (Universe u) => Universe (U u) where
  empty = D empty
  
  demoteD (D d) = d
  demoteD _ = empty

  shift Up   = shiftUp
  shift Down = shiftDown
  shift d    = umap (shift (demoteD d))

get :: (Universe u) => D u -> u c -> c
get d = extract . shift d

----------------------------------------------------------------------
---- Universes and Cells ---------------------------------------------
----------------------------------------------------------------------

-- class Cell u c | c -> u where
--   stepCell :: u c -> c


-- class ToChar c where
--   toChar :: c -> Char
