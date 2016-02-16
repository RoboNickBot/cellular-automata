{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Cellular.UStack
  ( C (..)
  , U (..)
  , UStack (..)
  , Dir (..)
  , mkList

  ) where

import Control.Comonad

----------------------------------------------------------------------

class UStack u where
  data Dir u
  emptyDir :: Dir u
  demoteDir :: Dir (U u) -> Dir u
  promoteDir :: Dir u -> Dir (U u)
  oppositeDir :: Dir u -> Dir u

  modFocus :: (c -> c) -> u c -> u c

  shift :: Dir u -> u c -> u c
  uniform :: c -> u c

  extract' :: u c -> c
  dup' :: UStack v => (Dir u -> Dir v) -> v c -> u c -> u (v c)


----------------------------------------------------------------------

data C c = C c
  deriving (Show, Eq, Ord)

instance Functor C where
  fmap f (C c) = C (f c)

instance UStack C where
  data Dir C = Base
  emptyDir = Base
  demoteDir _ = Base
  promoteDir = Stack
  oppositeDir _ = Base

  modFocus f (C c) = C (f c)

  shift _ = id
  uniform = C

  dup' _ v _ = C v
  extract' (C x) = x

instance Comonad C where
  extract = extract'
  duplicate c = C c


----------------------------------------------------------------------

data U u c = U [u c] (u c) [u c]
  deriving (Show, Eq, Ord)

shiftUp, shiftDown :: U u c -> U u c
shiftUp   (U (a:as) x     bs) = U     as a (x:bs)
shiftDown (U     as x (b:bs)) = U (x:as) b     bs

umap :: (u c -> v k) -> U u c -> U v k
umap f (U as x bs) = U (map f as) (f x) (map f bs)

infiniteCopies :: u c -> U u c
infiniteCopies u = U (repeat u) u (repeat u)

demote :: U u c -> u c
demote (U _ x _) = x

modUFocus :: (u c -> u c) -> U u c -> U u c
modUFocus f (U as x bs) = U as (f x) bs

mkList :: Int -> U u c -> [u c]
mkList r (U as c bs) = reverse (take r as) ++ [c] ++ take r bs

instance (Functor u) => Functor (U u) where                       
  fmap = umap . fmap

instance UStack u => UStack (U u) where
  data Dir (U u) = Stack (Dir u) | Up | Down
  emptyDir = Stack emptyDir

  demoteDir (Stack d) = d
  demoteDir _ = emptyDir
  
  promoteDir = Stack

  oppositeDir Up = Down
  oppositeDir Down = Up
  oppositeDir (Stack d) = Stack (oppositeDir d)

  modFocus f u = modUFocus (modFocus f) u

  shift Up   = shiftUp
  shift Down = shiftDown
  shift d    = umap (shift (demoteDir d))

  uniform c = infiniteCopies (uniform c)
  
  extract' = extract' . demote
  dup' f v (U as c bs) = U (spread (f . Stack) (f Up) v as) 
                           (dup' (f . Stack) v c)
                           (spread (f . Stack) (f Down) v bs)

spread :: (UStack v, UStack u) => (Dir u -> Dir v) -> Dir v -> v c -> [u c] -> [u (v c)]
spread f' dir v = zipWith (dup' f') (tail $ iterate (shift dir) v)

instance (UStack u, Functor u) => Comonad (U u) where
  extract = extract'
  duplicate v = dup' id v v


----------------------------------------------------------------------
-- quick hack zone --

unC :: C c -> c
unC (C c) = c

toList1 :: U C c -> [c]
toList1 (U _ x bs) = map unC (x:bs)

toList1Lim :: Int -> U C c -> [c]
toList1Lim i (U as x bs) = (reverse . take i . map unC) as
                           ++ [unC x] 
                           ++ (take i . map unC) bs

toList2Lim :: Int -> U (U C) c -> [[c]]
toList2Lim i (U as x bs) = (reverse . take i . map (toList1Lim i)) as
                           ++ [toList1Lim i x] 
                           ++ (take i . map (toList1Lim i)) bs

-- bigbang :: UStack u => c -> c -> u c
-- bigbang seed space = modFocus (const seed) $ uniform space
