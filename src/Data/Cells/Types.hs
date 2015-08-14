module Data.Cells.Types 
  ( Direction
  , Cell2Char(..)
  
  ) where

class (Show d, Eq d, Ord d) => Direction d where
  opposite :: d -> d
  self :: d

class Cell2Char c where
  cell2Char :: c -> Char
