module Data.Cells.Cells
  ( next

  ) where

import Control.Comonad

import Data.Cells.Types
import Data.Cells.Directions
import Data.Cells.Universes


next :: (Comonad u, Cell u c) => u c -> u c
next = fmap stepCell . duplicate

