{-# LANGUAGE TypeFamilies #-}
module Data.Cellular.Directions 
  ( demote
  , opposite
  , self
  
  ) where

import Data.Cellular.Types


-- These example axes build off each other...  Going further, could
-- higher dimensions of these be generated automatically?
data A1 = A1
data A2 = A2 | A2A1 A1
data A3 = A3 | A3A2 A2

data Z = Z deriving Show

data A a = A a deriving Show

data Something u = Something

type instance Axes (Something c) = A (A Z)

unstack :: Staxis a -> a
unstack (Staxis a) = a

demote' :: Axes u ~ Staxis (Axes v) -- <- OMFG I LOVE TypeFamilies !!!
        => DirSelect u -> DirSelect v
demote' (LeftSide a) = LeftSide (unstack a)
demote' (RightSide a) = RightSide (unstack a)

-- | Transforms a higher-axes type into the equivalent one step down
demote :: Axes u ~ Staxis (Axes v) => Direction u -> Direction v
demote = fmap demote'

-- | Gives the opposite direction
opposite :: Direction u -> Direction u
opposite = fmap (\sel -> case sel of
                           LeftSide a -> RightSide a
                           RightSide a -> LeftSide a)

-- | Gives the "identity direction", Nothing
self :: Direction u
self = Nothing 

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
