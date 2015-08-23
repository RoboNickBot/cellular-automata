{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}

module Cellular.Sidewalk
  ( Sidewalk
  , mkSidewalk
  , string
  , printSteps
  
  , pattern East
  , pattern West

  ) where

import Data.Cellular

----------------------------------------------------------------------

data Sidewalk c = Sidewalk [c] c [c]
  deriving (Show, Eq, Ord)

type instance Axes (Sidewalk c) = Axis

pattern East = RightSide Axis
pattern West = LeftSide Axis

----------------------------------------------------------------------

instance Universe Sidewalk where
  shift East (Sidewalk     as x (b:bs)) = Sidewalk (x:as) b     bs
  shift West (Sidewalk (a:as) x     bs) = Sidewalk     as a (x:bs)
  shift _ u = u -- For the Nothing case

instance Functor Sidewalk where
  fmap f (Sidewalk as x bs) = Sidewalk (fmap f as) (f x) (fmap f bs)

instance Comonad Sidewalk where
  extract (Sidewalk _ x _) = x
  duplicate u = Sidewalk (tail $ iterate (shift West) u) 
                         u 
                         (tail $ iterate (shift East) u)

----------------------------------------------------------------------

mkSidewalk :: [a] -> a -> [a] -> Sidewalk a
mkSidewalk = Sidewalk

string :: (ToChar c) => Int -> Sidewalk c -> String
string r (Sidewalk as x bs) = 
  let lefthand =  fmap toChar $ reverse (take r as)
      righthand = fmap toChar $ take r bs
  in lefthand ++ [toChar x] ++ righthand

printSteps :: (ToChar c, Cell Sidewalk c) 
           => Int -> Sidewalk c -> IO ()
printSteps r u = sequence_ $ take r $ fmap (putStrLn . string r) 
                                             (iterate next u)
