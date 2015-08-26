{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Cellular.Classes
  ( Universe (..)
  , Automaton (..)
  , next
  
  , Draw (..)

  ) where

import Control.Comonad

----------------------------------------------------------------------

class Universe u where
  step :: (u c -> c) -> u c -> u c

instance Comonad u => Universe u where
  step r = fmap r . duplicate

----------------------------------------------------------------------

class Automaton u c where
  rule :: u c -> c
  
next :: (Universe u, Automaton u c) => u c -> u c
next = step rule

----------------------------------------------------------------------

class Draw i c where
  draw :: c -> i
  

