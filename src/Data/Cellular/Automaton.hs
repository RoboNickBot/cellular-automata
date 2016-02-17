{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Cellular.Automaton
  (
 
    Automaton (..)
  , next

  ) where

import Data.Cellular.Universe

----------------------------------------------------------------------

class Automaton u c where
  rule :: u c -> c

next :: (Universe u, Automaton u c) => u c -> u c
next = step rule
