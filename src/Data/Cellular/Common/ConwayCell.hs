{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Cellular.Common.ConwayCell where

import Data.Cellular
import qualified Data.Cellular.Common.Sidewalk as SW
import Data.Cellular.Common.Chessboard

newtype ConwayCell = ConwayCell { conwayCellVal :: Bool }

instance Cell Chessboard ConwayCell where
  stepCell = life
  
-- these update functions could make use of a reader monad
life :: Chessboard ConwayCell -> ConwayCell
life u = let n = get North u
             ne = conwayCellVal . extract . shift North . shift East $ u
             nw = conwayCellVal . extract . shift North . shift West $ u
             s = get South u
             se = conwayCellVal . extract . shift South . shift East $ u
             sw = conwayCellVal . extract . shift South . shift West $ u
             w = get West u
             e = get East u
             c = get Nothing u
             liveNs = length $ filter id [n,ne,nw,s,se,sw,w,e]
         in if c
               then if liveNs < 2
                       then scOff
                       else if liveNs > 3
                               then scOff
                               else scOn
               else if liveNs == 3
                       then scOn
                       else scOff

get :: Direction (Chessboard ConwayCell) 
    -> Chessboard ConwayCell
    -> Bool
get d = conwayCellVal . extract . shift d

instance ToChar ConwayCell where
  toChar (ConwayCell True) = '#'
  toChar _ = '\''

scOff, scOn :: ConwayCell
scOff = ConwayCell False
scOn = ConwayCell True
