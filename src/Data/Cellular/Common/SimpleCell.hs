{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Cellular.Common.SimpleCell where

import Data.Cellular
import Data.Cellular.Common.Sidewalk


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
