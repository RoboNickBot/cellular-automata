{-# LANGUAGE FlexibleInstances #-}

module Data.Cellular.Universe.Sidewalk
  ( 
  
    Sidewalk (..)
  , left
  , right

  ) where
  
import Data.Cellular
import Data.Cellular.UStack

type Sidewalk = U C

right :: Path Sidewalk
right = fromDStack Down

left :: Path Sidewalk
left = fromDStack Up


