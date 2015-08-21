{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Cellular.Common.Chessboard where

import Data.Cellular
import qualified Data.Cellular.Common.Sidewalk as SW

----------------------------------------------------------------------

data Chessboard c = Chessboard [SW.Sidewalk c] 
                               (SW.Sidewalk c) 
                               [SW.Sidewalk c]
  deriving (Show, Eq, Ord)
  
type instance Axes (Chessboard c) = Staxis (Axes (SW.Sidewalk c))

pattern North = Just (LeftSide Topxis)
pattern South = Just (RightSide Topxis)
pattern East = Just (LeftSide (Staxis Axis))
pattern West = Just (RightSide (Staxis Axis))

instance Universe Chessboard where
  shift North (Chessboard (l:ls) c rs) = Chessboard ls l (c:rs)
  shift South (Chessboard ls c (r:rs)) = Chessboard (c:ls) r rs
  shift d (Chessboard l c r) = 
    let s = (shift . demote) d
    in Chessboard (map s l) (s c) (map s r)
