module Cellular.Common where

import Data.Cellular

----------------------------------------------------------------------

type U0 = C

type U1 = U U0

right, left :: Loc U1
right = loc Down
left = loc Up

type U2 = U U1

north, south, east, west :: Loc U2
north = loc Up
south = loc Down

east = loc (Stack Down)
west = loc (Stack Up)

northEast = north <> east
northWest = north <> west
southEast = south <> east
southWest = south <> west

adjacents :: [Loc U2]
adjacents = [north, south, east, west]

diagonals :: [Loc U2]
diagonals = [northEast, northWest, southEast, southWest]

neighbors :: [Loc U2]
neighbors = adjacents ++ diagonals

type U3 = U U2
