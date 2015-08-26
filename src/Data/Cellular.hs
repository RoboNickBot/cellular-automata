module Data.Cellular
  ( module Control.Comonad
  , (<>)
  
  , module Data.Cellular.Classes
  , module Data.Cellular.UStack
  
  , Loc
  , loc
  , get
  , self

  ) where


import Control.Comonad

import Data.Monoid

import Data.Monoid.Extractor
import Data.Cellular.Classes
import Data.Cellular.UStack

type Loc = Extractor

loc d = mkExtractor $ getFrom d

get :: UStack u => Loc u -> u c -> c
get l = applyExtractor l

self :: UStack u => Loc u
self = loc emptyDir
