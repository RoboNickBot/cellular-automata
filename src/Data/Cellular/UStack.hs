{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Cellular.UStack 
  ( C (..)
  , U (..)
  , UStack (..)) where

import Control.Comonad

import Data.Cellular.Direction
import Data.Cellular.Universe

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

class Comonad u => UStack u where
  data DStack u
  emptyDir :: DStack u
  demoteDir :: DStack (U u) -> DStack u
  shiftU :: DStack u -> u c -> u c

instance UStack C where
  data DStack C = Base
  emptyDir = Base
  demoteDir _ = Base
  shiftU _ = id

instance (UStack u) => UStack (U u) where
  data DStack (U u) = Stack (DStack u) | Up | Down
  emptyDir = Stack emptyDir

  demoteDir (Stack d) = d
  demoteDir _ = emptyDir

  shiftU Up   = shiftUp
  shiftU Down = shiftDown
  shiftU d    = umap (shiftU (demoteDir d))

----------------------------------------------------------------------

instance (UStack u) => Universe u where
  data DirectionType u = DStack (DStack u)
  mkDir (DStack d) = Direction $ extract . shiftU d

----------------------------------------------------------------------
---- Universes and Cells ---------------------------------------------
----------------------------------------------------------------------

-- class Cell u c | c -> u where
--   stepCell :: u c -> c


-- class ToChar c where
--   toChar :: c -> Char

