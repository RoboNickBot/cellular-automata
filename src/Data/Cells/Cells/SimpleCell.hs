{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Cells.Cells.SimpleCell where

import Control.Comonad

import Data.Cells.Types
import Data.Cells.Directions
import Data.Cells.Universes
import Data.Cells.Cells
import Data.Cells.Universes.Sidewalk


newtype SimpleCell = SimpleCell { simpleCellVal :: Bool }

instance Cell Sidewalk SimpleCell where
  stepCell u = 
    let lx = (simpleCellVal . extract . shift Up) u
        cx = (simpleCellVal . extract) u
    in SimpleCell (lx /= cx)

instance ToChar SimpleCell where
  toChar (SimpleCell True) = '#'
  toChar _ = '\''

scOff, scOn :: SimpleCell
scOff = SimpleCell False
scOn = SimpleCell True

testSimpleCells = mkSidewalk (repeat scOff) 
                             scOn 
                             (repeat scOff)
