{-# LANGUAGE Rank2Types #-}

module Data.Monoid.Extractor 
  ( Extractor (..)
  , mkExtractor
  
  ) where

import Control.Comonad

----------------------------------------------------------------------

-- This is definitely a re-invention of something, but I've given up
-- trying to figure what out for now.
--
-- Leads to study: Lens, Comonad State, Comonad Env, Comonad Trace,
-- Kleisli Arrow, co-Kleisli Arrow

newtype Extractor u = 
  Extractor { applyExtractor :: forall c. u c -> c }

mkExtractor = Extractor

instance Comonad u => Monoid (Extractor u) where
  mempty = Extractor extract
  mappend g h = let a = applyExtractor
                in mkExtractor (a h . extend (a g))


