{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Cellular.Universe
  (
  
    Universe (..)
  , step
  , opposite
  , here
  , move
  , look
  , at
  , modify
  , set
  
  , initPattern

  , Path (Path)
  , path  
  
  ) where

import Control.Comonad

----------------------------------------------------------------------

class Comonad u => Universe u where
  data Dir u

  flipDir :: Dir u -> Dir u
  nullDir :: Dir u

  shift :: Dir u -> u c -> u c
  modFocus :: (c -> c) -> u c -> u c
  uniform :: c -> u c

step :: Universe u => (u c -> c) -> u c -> u c
step r = fmap r . duplicate

newtype Path u = Path [Dir u] deriving Monoid

path :: Dir u -> Path u
path d = Path [d]

opposite :: Universe u => Path u -> Path u
opposite (Path ds) = Path (map flipDir ds)

here :: Universe u => Path u
here = Path [nullDir]

move :: Universe u => Path u -> u c -> u c
move (Path ds) u = foldr shift u ds

look :: Universe u => Path u -> u c -> c
look p = extract . move p

at :: Universe u => Path u -> (u c -> u c) -> u c -> u c
at p f = move (opposite p) . f . move p

modify :: Universe u => (c -> c) -> u c -> u c
modify = modFocus

set :: Universe u => c -> u c -> u c
set c = modify (const c)

----------------------------------------------------------------------

initPattern :: Universe u => c -> c -> [Path u] -> u c
initPattern off on = foldr (\p -> at p $ set on) (uniform off)
