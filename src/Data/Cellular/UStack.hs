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

repeatU :: u c -> U u c
repeatU u = U (repeat u) u (repeat u)

promote :: U u c -> U (U u) c
promote u = let ds f = tail . iterate f
            in U (ds shiftUp u) u (ds shiftDown u)

demote :: U u c -> u c
demote (U _ x _) = x

instance (Functor u) => Functor (U u) where                       
  fmap = umap . fmap

instance (Comonad u) => Comonad (U u) where
  extract = extract . demote
  duplicate = umap (\u -> u <$ demote u) . promote

----------------------------------------------------------------------

class Comonad u => UStack u where
  data DStack u
  emptyDir :: DStack u
  demoteDir :: DStack (U u) -> DStack u
  shift' :: DStack u -> u c -> u c
  uniform' :: c -> u c

instance UStack C where
  data DStack C = Base
  emptyDir = Base
  demoteDir _ = Base
  shift' _ = id
  uniform' = C

instance (UStack u) => UStack (U u) where
  data DStack (U u) = Stack (DStack u) | Up | Down
  emptyDir = Stack emptyDir

  demoteDir (Stack d) = d
  demoteDir _ = emptyDir

  shift' Up   = shiftUp
  shift' Down = shiftDown
  shift' d    = umap (shift' (demoteDir d))

  uniform' c = repeatU (uniform' c)

----------------------------------------------------------------------

instance (UStack u) => Universe u where
  data DirectionType u = DStack (DStack u)
  mkDir (DStack d) = Direction $ extract . shift' d
  uniform = uniform'
