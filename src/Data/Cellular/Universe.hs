{-# LANGUAGE TypeFamilies #-}

module Data.Cellular.Universe
  (
  
    Universe (..)
  , step
  , opposite
  , move
  , look
  , at
  , modify
  , set
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

step :: Universe u => (u c -> c) -> u c -> u c
step r = fmap r . duplicate

newtype Path u = Path [Dir u]

path :: Dir u -> Path u
path d = Path [d]

opposite :: Universe u => Path u -> Path u
opposite (Path ds) = Path (map flipDir ds)

move :: Universe u => Path u -> u c -> u c
move (Path ds) u = foldl (flip shift) u ds

look :: Universe u => Path u -> u c -> c
look p = extract . move p

at :: Universe u => Path u -> (u c -> u c) -> u c -> u c
at p f = move (opposite p) . f . move p

modify :: Universe u => (c -> c) -> u c -> u c
modify = modFocus

set :: Universe u => c -> u c -> u c
set c = modify (const c)
