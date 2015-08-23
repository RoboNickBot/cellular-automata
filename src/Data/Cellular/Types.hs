{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Cellular.Types 
  ( Axis (..), Staxis (..) 
  , Axes (..)
  , DirSelect (..)
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

data DirSelect u = LeftSide (Axes u) | RightSide (Axes u)


type Direction u = Maybe (DirSelect u)


----------------------------------------------------------------------
---- Universes and Cells ---------------------------------------------
----------------------------------------------------------------------

class Universe u where
  shift :: Direction (u c) -> u c -> u c

class Cell u c | c -> u where
  stepCell :: u c -> c


class ToChar c where
  toChar :: c -> Char
