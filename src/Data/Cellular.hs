{-# LANGUAGE FlexibleContexts #-}

module Data.Cellular
  ( module Control.Comonad
  , (<>)
  
  , module Data.Cellular.Classes
  , module Data.Cellular.UStack
  
  , Loc
  , loc
  , get
  , self
  , toStringU2

  ) where


import Control.Comonad
import Data.Monoid
import Data.List (intersperse)

import Data.Monoid.Extractor
import Data.Cellular.Classes
import Data.Cellular.UStack

type Loc = Extractor

loc d = mkExtractor $ getFrom d

get :: UStack u => Loc u -> u c -> c
get l = applyExtractor l

self :: UStack u => Loc u
self = loc emptyDir

toStringU2 :: (Draw Char c) => Int -> U (U C) c -> String
toStringU2 i = concat 
               . intersperse "\n" 
               . toList2Lim i
               . fmap draw

