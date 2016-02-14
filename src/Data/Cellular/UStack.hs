{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE TypeFamilies #-}

module Data.Cellular.UStack
  ( C, U
  , UStack (..)
  , DStack (..)
  
  -- , getFrom, setAt
  -- , toList1Lim
  -- , toList2Lim
  -- , bigbang

  ) where

import Control.Comonad

-- import Data.Cellular.Classes

----------------------------------------------------------------------

data C c = C c
  deriving (Show, Eq, Ord)

instance Functor C where
  fmap f (C c) = C (f c)

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

instance (Functor u) => Functor (U u) where                       
  fmap = umap . fmap

----------------------------------------------------------------------

class Functor u => UStacker u where
  data DStack u
  emptyDir :: DStack u
  demoteDir :: DStack (U u) -> DStack u
  oppositeDir :: DStack u -> DStack u
  
  modFocus :: (c -> c) -> u c -> u c

  shift :: DStack u -> u c -> u c
  uniform :: c -> u c

  extract' :: u c -> c
  duplicate' :: u c -> u (u c)

instance UStacker C where
  data DStack C = Base
  emptyDir = Base
  demoteDir _ = Base
  oppositeDir _ = Base

  modFocus f (C c) = C (f c)

  shift _ = id
  uniform = C
  
  extract' (C x) = x
  duplicate' c = C c

instance UStacker u => UStacker (U u) where
  data DStack (U u) = Stack (DStack u) | Up | Down
  emptyDir = Stack emptyDir

  demoteDir (Stack d) = d
  demoteDir _ = emptyDir

  oppositeDir Up = Down
  oppositeDir Down = Up
  oppositeDir (Stack d) = Stack (oppositeDir d)

  modFocus f u = modUFocus (modFocus f) u

  shift Up   = shiftUp
  shift Down = shiftDown
  shift d    = umap (shift (demoteDir d))

  uniform c = infiniteCopies (uniform c)
  
  extract' = extract' . demote
  duplicate' u = let ud = umap duplicate' u
                     ups = tail $ iterate (shift Up) ud
                     downs = tail $ iterate (shift Down) ud
                     ds f = tail . map (umap extract') . iterate f
                 in undefined

data UStacker s => UStack s c = UStack { uStacker :: s c }

instance UStacker s => Functor (UStack s) where
  fmap f = UStack . fmap f . uStacker

instance UStacker s => Comonad (UStack s) where
  extract = extract' . uStacker
  duplicate = undefined

-- instance (Comonad u) => Comonad (U u) where
--   extract = extract . demote
--   duplicate = umap (\u -> fmap (const u) (demote u)) . promote

-- promote :: (Comonad u) => U u c -> U (U u) c
-- promote u = 
--   let ds f = tail . map (umap extract) . iterate f

--       -- The type-checker couldn't deal with a type signature for
--       -- 'ud', but this should be correct: "ud :: U u (u c)"
--       ud = umap duplicate u
--       ups = tail $ iterate shiftUp ud
--       downs = tail $ iterate shiftDown ud

--   in U (ds shiftUp ud) u (ds shiftDown ud)

-- instance Comonad C where
--   extract (C x) = x
--   duplicate c = C c


-- getFrom :: UStack u => DStack u -> u c -> c
-- getFrom d = extract . shift d

-- -- I suspect there is a more comonadic way to do setting though...
-- setAt :: UStack u => DStack u -> c -> u c -> u c
-- setAt d c u = shift (oppositeDir d) (modFocus (const c) (shift d u))

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
