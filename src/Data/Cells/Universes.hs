module Data.Cells.Universes 
  ( Universe1D
  , mkUniverse1D
  , left1D, right1D
  , extract1D
  , duplicate1D
  , string1D
  , print1D
  , next1D
  , Cell1D(..)

  ) where

import Control.Comonad

import Data.Cells.Types


data Universe1D a = Universe1D [a] a [a]
  deriving (Show, Eq, Ord)

left1D, right1D :: Universe1D a -> Universe1D a
left1D (Universe1D (a:as) x bs) = Universe1D as a (x:bs)
right1D (Universe1D as x (b:bs)) = Universe1D (x:as) b bs

extract1D :: Universe1D a -> a
extract1D (Universe1D _ x _) = x

duplicate1D :: Universe1D a -> Universe1D (Universe1D a)
duplicate1D u = Universe1D (tail $ iterate left1D u) 
                           u 
                           (tail $ iterate right1D u)

instance Functor Universe1D where
  fmap f (Universe1D as x bs) = 
    Universe1D (fmap f as) (f x) (fmap f bs)

instance Comonad Universe1D where
  extract = extract1D
  duplicate = duplicate1D

class Cell1D c where
  stepCell :: Universe1D c -> c

next1D :: (Cell1D c) => Universe1D c -> Universe1D c
next1D u = fmap stepCell (duplicate1D u)

string1D :: (ToChar c) => Int -> Universe1D c -> String
string1D r (Universe1D as x bs) = 
  let lefthand =  fmap toChar $ reverse (take r as)
      righthand = fmap toChar $ take r bs
  in lefthand ++ [toChar x] ++ righthand

print1D :: (ToChar c, Cell1D c) => Int -> Universe1D c -> IO ()
print1D r u = sequence_ $ take r $ fmap (putStrLn . string1D r) 
                                        (iterate next1D u)

mkUniverse1D :: [a] -> a -> [a] -> Universe1D a
mkUniverse1D = Universe1D
