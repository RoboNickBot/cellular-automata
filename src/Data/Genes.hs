{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}

module Data.Genes where

import Data.Word8 (Word8)

-- Just to make things compile; this will have to be updated later
--- import Data.Cells (Direction)
class Direction d


-- TODO: class Signal
newtype Signal = Signal Word8 deriving (Show, Eq, Ord)

class (Direction d, Show m, Eq m, Ord m)
  => DirectionMap m d s | d -> m 

  where emptyDM  :: m
        lookupDM :: m -> d -> s
        insertDM :: m -> d -> s -> m

data (DirectionMap m d Signal) 
  => SignalMap m d = SignalMap { sense :: m }

instance (DirectionMap m d Signal) => Eq (SignalMap m d) where
  (==) (SignalMap a) (SignalMap b) = a == b

newtype (Direction d) 
  => Focus d = Focus d deriving (Show, Eq, Ord)

data (DirectionMap m d Signal) => GeneSpec m d = 
  GeneSpec 
    ( SignalMap m d -> (Focus d, SignalMap m d) )
