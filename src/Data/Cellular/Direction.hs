{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Cellular.Direction 
  ( Direction (..)
  
  ) where

import Control.Comonad

----------------------------------------------------------------------

-- TODO: Look around and see if this has already been described and
-- implemented by someone else; it seems very general.

-- TODO: Make Direction usable as a getter and setter; it looks like
-- this Direction thing is really just a Lens

-- TODO: On that note, I suspect that Universes here have something to
-- do with "Comonad Stores"?

newtype Direction u = Direction { get :: forall c. u c -> c }

instance (Comonad u) => Monoid (Direction u) where
  mempty = Direction extract
  mappend (Direction g) (Direction h) = Direction (h . extend g)

