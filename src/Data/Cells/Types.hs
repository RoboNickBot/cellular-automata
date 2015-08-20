module Data.Cells.Types 
  ( ToChar(..)
  
  ) where

class ToChar c where
  toChar :: c -> Char
