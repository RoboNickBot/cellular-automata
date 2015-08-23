{-# LANGUAGE MultiParamTypeClasses #-}

module Cellular.SimpleCell where

import Data.Cellular
import Cellular.Sidewalk


newtype SimpleCell = SimpleCell { simpleCellVal :: Bool }

instance Cell Sidewalk SimpleCell where
  stepCell u = 
    let lx = (simpleCellVal . extract . shift West) u
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
