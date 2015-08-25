{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Cellular.Classes 
  ( Universe (get)
  , Direction
  , shift
  
  ) where

import Control.Comonad


class (Comonad u) => Universe u where
  data Direction u
  get :: Direction u -> u c -> c  

shift :: (Universe u) => Direction u -> u c -> u c
shift d = extend (get d)

newtype Get u c = Get { runGet :: u c -> c }

instance (Comonad u) => Monoid (Get u c) where
  mempty = Get extract
  mappend (Get g) (Get h) = Get (h . extend g)
