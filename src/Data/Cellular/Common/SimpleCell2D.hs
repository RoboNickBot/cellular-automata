{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Cellular.Common.SimpleCell2D where

import Data.Cellular
import qualified Data.Cellular.Common.Sidewalk as SW
import Data.Cellular.Common.Chessboard


newtype SimpleCell2D = SimpleCell2D { simpleCell2DVal :: Bool }

instance Cell Chessboard SimpleCell2D where
  stepCell u = let nx = get North u
                   sx = get South u
                   wx = get West u
                   ex = get East u
                   cx = get self u
               in if wx || nx
                     then SimpleCell2D True
                     else SimpleCell2D False

get :: Direction (Chessboard SimpleCell2D) 
    -> Chessboard SimpleCell2D 
    -> Bool
get d = simpleCell2DVal . extract . shift d

instance ToChar SimpleCell2D where
  toChar (SimpleCell2D True) = '#'
  toChar _ = '\''

scOff, scOn :: SimpleCell2D
scOff = SimpleCell2D False
scOn = SimpleCell2D True

testSimpleCells2D = mkChessboard (repeat others) 
                                 center 
                                 (repeat others)

center = SW.mkSidewalk (repeat scOff) 
                       scOn 
                       (repeat scOff)
                       
others = SW.mkSidewalk (repeat scOff)
                       scOff
                       (repeat scOff)

applyN :: Int -> (a -> a) -> a -> a
applyN n f a = if n > 0
                  then applyN (n - 1) f (f a)
                  else a
