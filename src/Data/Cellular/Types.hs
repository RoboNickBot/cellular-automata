{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Cellular.Types 
  ( Axis (..), Staxis (..) 
  , Axes (..)
  , Direction (..)
  
  , Universe (..)
  , Cell (..)

  , ToChar (..)
  
  ) where

import Control.Comonad


----------------------------------------------------------------------
---- Direction stuff -------------------------------------------------
----------------------------------------------------------------------

-- multi-axis building blocks!

-- TODO: This is probably already some mathematically well-defined
-- type in some other library; find out what it is and use it instead
data Axis = Axis
data Staxis a = Staxis a | Topxis

type family Axes u

data Direction u = LeftSide (Axes u) 
                 | RightSide (Axes u) 
                 | NoDirection

----------------------------------------------------------------------

-- Possible alternate definition for Universe, that doesn't require a
-- type family?  But how would I go about associating Directions to
-- this?...
data U u = U [u] u [u]

data Cell' = Cell'

type U1 = U Cell'
type U2 = U (U Cell')
type U3 = U (U (U Cell'))

-- What about this:
data U' c = U' [U' c] (U' c) [U' c] | Base' c
-- Now, all universes have the same type, and can be constructed to
-- different numbers of dimensions as needed based on the
-- differently-typed Direction types.
-- 
-- Nah, I don't like that all universes have the same type.

----------------------------------------------------------------------

data Stack a = Stack a | Focus

data Orientation a = Up a | Down a | None


data Proxy a = Proxy

proxy :: a -> Proxy a
proxy a = Proxy :: (Proxy a)

class Shift n where
  type Universe' n c
  shift' :: Proxy c -- why does this have to be 'c' and not 'n'?
         -> Orientation n -- make the 'n' here the result of Dir
                          -- (Universe' n c), where Dir is a Type
                          -- Family, to get around the non-injective
                          -- type function problem?  This way, if we
                          -- have (Universe' n c), then we can know
                          -- for sure what n produced it (note: this
                          -- may be impossible?) also, it's 'c' that's
                          -- the problem, not 'n'
         -> Universe' n c
         -> Universe' n c

instance Shift () where
  type Universe' () c = c
  shift' _ _ = id

instance (Shift n, Demote n) => Shift (Stack n) where
  type Universe' (Stack n) c = ( [Universe' n c]
                               , Universe' n c
                               , [Universe' n c] )
  shift' _ None u = u
  shift' _ (Up Focus) (a:as, x, bs) = (as, a, x:bs)
  shift' _ (Down Focus) (as, x, b:bs) = (x:as, b, bs)
  shift' p o (as, x, bs) = let s = shift' p (demoteO o)
                           in (map s as, s x, map s bs)

class Demote n where
  demoteO :: Orientation (Stack n) -> Orientation n

instance Demote (Stack n) where
  demoteO (Up (Stack (Stack n))) = (Up (Stack n))
  demoteO (Down (Stack (Stack n))) = (Down (Stack n))

-- demote' :: u ~ Stack v => Orientation u -> Orientation v
-- demote' (Up (Stack a)) = Up a
-- demote' (Down (Stack a)) = Down a
-- demote' _ = None

----------------------------------------------------------------------
---- Universes and Cells ---------------------------------------------
----------------------------------------------------------------------

class Universe u where
  shift :: Direction (u c) -> u c -> u c

class Cell u c | c -> u where
  stepCell :: u c -> c


class ToChar c where
  toChar :: c -> Char
