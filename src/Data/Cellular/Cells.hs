module Data.Cellular.Cells
  ( next

  ) where

import Control.Comonad

import Data.Cellular.Types
import Data.Cellular.Directions
import Data.Cellular.Universes


next :: (Comonad u, Cell u c) => u c -> u c
next = fmap stepCell . duplicate

