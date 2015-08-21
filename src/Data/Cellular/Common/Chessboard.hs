{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Cellular.Common.Chessboard 
  ( Chessboard
  , mkChessboard
  , string
  , animate
  
  , pattern North
  , pattern South
  , pattern East
  , pattern West
  
  ) where

import Control.Concurrent

import Data.Cellular
import qualified Data.Cellular.Common.Sidewalk as SW

----------------------------------------------------------------------

data Chessboard c = Chessboard [SW.Sidewalk c] 
                               (SW.Sidewalk c) 
                               [SW.Sidewalk c]
  deriving (Show, Eq, Ord)
  
mkChessboard = Chessboard

type instance Axes (Chessboard c) = Staxis (Axes (SW.Sidewalk c))

pattern North = Just (LeftSide Topxis)
pattern South = Just (RightSide Topxis)
pattern East = Just (RightSide (Staxis Axis))
pattern West = Just (LeftSide (Staxis Axis))

----------------------------------------------------------------------

instance Universe Chessboard where
  shift North (Chessboard (l:ls) c rs) = Chessboard ls l (c:rs)
  shift South (Chessboard ls c (r:rs)) = Chessboard (c:ls) r rs
  shift d (Chessboard ls c rs) = 
    let s = (shift . demote) d
    in Chessboard (map s ls) (s c) (map s rs)

instance Functor Chessboard where
  fmap f (Chessboard ls c rs) = Chessboard (map (fmap f) ls) 
                                           (fmap f c) 
                                           (map (fmap f) rs)
                                           
instance Comonad Chessboard where
  extract (Chessboard _ c _) = extract c
  duplicate u = Chessboard (dshift North u) 
                           (dupSlice u) 
                           (dshift South u)

dshift d = map dupSlice . tail . iterate (shift d)

dupSlice :: Chessboard c -> SW.Sidewalk (Chessboard c)
dupSlice u = SW.mkSidewalk (tail $ iterate (shift West) u)
                           u
                           (tail $ iterate (shift East) u)
                           
----------------------------------------------------------------------

animate :: (ToChar c, Cell Chessboard c) 
        => Int -> Int -> Chessboard c -> IO ()
animate size delay u = do putStrLn ""
                          putStrLn (string size u)
                          putStrLn ""
                          putStrLn (take (size + 2) (repeat '='))
                          
                          threadDelay (delay * 1000)
                          animate size delay (next u)
                          
string :: (ToChar c) => Int -> Chessboard c -> String
string size (Chessboard ls c rs) = 
  concat (map (++ "\n") $ take size (map (SW.string size) ls))
  ++ SW.string size c
  ++ "\n"
  ++ concat (map (++ "\n") $ take size (map (SW.string size) rs))
