{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Cellular.Types 
  ( Axes (..)
  , Select (..)
  , Direction (..)
  
  , Universe (..)
  , Cell (..)

  , ToChar (..)
  
  ) where

import Control.Comonad


----------------------------------------------------------------------
---- Direction stuff -------------------------------------------------
----------------------------------------------------------------------

type family Axes u

data Select u = LeftSide (Axes u) | RightSide (Axes u)

type Direction u = Maybe (Select u)


----------------------------------------------------------------------
---- Universes and Cells ---------------------------------------------
----------------------------------------------------------------------

class Universe u where
  shift :: Direction (u c) -> u c -> u c

class Cell u c | c -> u where
  stepCell :: u c -> c



class ToChar c where
  toChar :: c -> Char
