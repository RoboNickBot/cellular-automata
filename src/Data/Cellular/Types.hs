{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Cellular.Types 
  ( Axis (..), Staxis (..) 
  , Axes (..)
  , Direction (..)
  
  , Universe (..)
  , Cell (..)

  , ToChar (..)
  
  ) where

import Control.Comonad


----------------------------------------------------------------------
---- Direction stuff -------------------------------------------------
----------------------------------------------------------------------

-- multi-axis building blocks!

-- TODO: This is probably already some mathematically well-defined
-- type in some other library; find out what it is and use it instead
data Axis = Axis
data Staxis a = Staxis a | Topxis

type family Axes u

data Direction u = LeftSide (Axes u) 
                 | RightSide (Axes u) 
                 | NoDirection

data Stack a = Stack a | Focus

data Orientation a = Up a | Down a | None

-- type family Universe' n c

class Shift n where
  type Universe' n
  shift' :: Orientation n -> Universe' n -> Universe' n

instance Shift () where
  type Universe' () = Int
  shift' _ u = u

instance (Shift n, Demote n) => Shift (Stack n) where
  type Universe' (Stack n) = ( [Universe' n]
                             , Universe' n
                             , [Universe' n] )
  shift' None u = u
  shift' (Up Focus) (a:as, x, bs) = (as, a, x:bs)
  shift' (Down Focus) (as, x, b:bs) = (x:as, b, bs)
  shift' o (as, x, bs) = let s = shift' (demoteO o)
                         in (map s as, s x, map s bs)

-- class Shift n c where
--   shift' :: Orientation n -> Universe' n c -> Universe' n c 
  
-- instance Shift () c where
--   shift' _ = id
  
-- shift' :: Orientation (Stack n) -> Universe' (Stack n) c -> Universe' (Stack n) c
-- shift' None u = u
-- shift' (Up Focus) (a:as, x, bs) = (as, a, x:bs)
-- shift' (Down Focus) (as, x, b:bs) = (x:as, b, bs)
-- shift' o (as, x, bs) = let s = shift' (demoteO o)
--                        in (map s as, s x, map s bs)

class Demote n where
  demoteO :: Orientation (Stack n) -> Orientation n

instance Demote (Stack n) where
  demoteO (Up (Stack (Stack n))) = (Up (Stack n))
  demoteO (Down (Stack (Stack n))) = (Down (Stack n))

-- demote' :: u ~ Stack v => Orientation u -> Orientation v
-- demote' (Up (Stack a)) = Up a
-- demote' (Down (Stack a)) = Down a
-- demote' _ = None

----------------------------------------------------------------------
---- Universes and Cells ---------------------------------------------
----------------------------------------------------------------------

class Universe u where
  shift :: Direction (u c) -> u c -> u c

class Cell u c | c -> u where
  stepCell :: u c -> c


class ToChar c where
  toChar :: c -> Char
