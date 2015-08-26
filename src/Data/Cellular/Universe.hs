{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Cellular.Universe
  ( Universe (..)
  
  ) where

import Control.Comonad

----------------------------------------------------------------------

class Universe u where
  step :: (u c -> c) -> u c -> u c

instance Comonad u => Universe u where
  step r = fmap r . duplicate

