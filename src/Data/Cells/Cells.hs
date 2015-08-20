module Data.Cells.Cells
  ( SimpleCell
  , scOff, scOn
  , testSimpleCells

  ) where

import Data.Cells.Types
import Data.Cells.Universes

newtype SimpleCell = 
  SimpleCell { simpleCellVal :: Bool }

instance Cell1D SimpleCell where
  stepCell u = 
    let lx = (simpleCellVal . extract1D . left1D) u
        cx = (simpleCellVal . extract1D) u
    in SimpleCell (lx /= cx)

instance ToChar SimpleCell where
  toChar (SimpleCell True) = '#'
  toChar _ = '\''

scOff, scOn :: SimpleCell
scOff = SimpleCell False
scOn = SimpleCell True

testSimpleCells = mkUniverse1D (repeat scOff) 
                               scOn 
                               (repeat scOff)
