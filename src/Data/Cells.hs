module Data.Cells (Direction) where

class (Show d, Eq d, Ord d) => Direction d where
  opposite :: d -> d
  self :: d
