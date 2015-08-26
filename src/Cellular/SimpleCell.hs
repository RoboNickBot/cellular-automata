{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Cellular.SimpleCell where

import Data.Cellular
import Cellular.Common

----------------------------------------------------------------------

newtype SimpleCell = SimpleCell { val :: Bool }

instance Automaton U1 SimpleCell where
  rule u = let lx = val $ get left u
               cx = val $ get self u
           in SimpleCell (lx /= cx)

instance Draw Char SimpleCell where
  draw (SimpleCell True) = '#'
  draw _ = '\''

scOff, scOn :: SimpleCell
scOff = SimpleCell False
scOn = SimpleCell True


