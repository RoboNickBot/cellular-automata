{-# LANGUAGE FlexibleInstances #-}

module Data.Cellular.Universe.Chessboard
  ( 

    Chessboard (..)
    
  , north
  , south
  , east
  , west

  , adjacent

  , northeast
  , northwest
  , southeast
  , southwest
  
  , diagonal

  ) where

import Data.Monoid ((<>))

import Data.Cellular
import Data.Cellular.UStack
import Data.Cellular.Universe.Sidewalk

type Chessboard = U Sidewalk

north :: Path Chessboard
north = fromDStack Up

south :: Path Chessboard
south = fromDStack Down

east :: Path Chessboard
east = promotePath right

west :: Path Chessboard
west = promotePath left

adjacent :: [Path Chessboard]
adjacent = [north, south, east, west]

northeast :: Path Chessboard
northeast = north <> east

northwest :: Path Chessboard
northwest = north <> west

southeast :: Path Chessboard
southeast = south <> east

southwest :: Path Chessboard
southwest = south <> west

diagonal :: [Path Chessboard]
diagonal = 
  [northeast, northwest, southeast, southwest]
