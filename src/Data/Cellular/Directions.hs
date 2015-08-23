{-# LANGUAGE TypeFamilies #-}

module Data.Cellular.Directions 
  ( demote
  , opposite
  , self
  
  ) where

import Data.Cellular.Types

-- | Transforms a higher-axes type into the equivalent one step down
demote :: Axes u ~ Staxis (Axes v) -- <- OMFG I LOVE TypeFamilies !!!
        => Direction u -> Direction v
demote (LeftSide (Staxis a)) = LeftSide a
demote (RightSide (Staxis a)) = RightSide a
demote _ = NoDirection


-- | Gives the opposite direction
opposite :: Direction u -> Direction u
opposite (LeftSide a) = RightSide a
opposite (RightSide a) = LeftSide a
opposite _ = NoDirection

-- | Gives the "identity direction", NoDirection
self :: Direction u
self = NoDirection

-- TODO: I'd like to have functions that turn the direction by one
-- step at a time.  These would be easy on a case-by-case basis for
-- each concrete Direction, but I'd really like to automatically
-- generate their implemenations.

-- I'd imagine this would require some distiction between planes.  For
-- example, the 3D world has three axes, but "turning left" over and
-- over would never turn onto the Up-Down axis.  However, if I made a
-- Universe of hexagonal tiles there would *also* be three axes, but
-- "turning left" would eventually turn through all three, because
-- they are in the same plane.

-- How would I add planes into the stacking Axes model?
