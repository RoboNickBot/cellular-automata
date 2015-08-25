{-# LANGUAGE TypeFamilies #-}

module Data.Cellular.Universe 
  (Universe (..)
  , shift
  
  ) where

import Control.Comonad

import Data.Cellular.Direction

class (Comonad u) => Universe u where
  data DirectionType u
  mkDir :: DirectionType u -> Direction u
  uniform :: c -> u c

shift :: (Comonad u) => Direction u -> u c -> u c
shift d = extend (get d)
