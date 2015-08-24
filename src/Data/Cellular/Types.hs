{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Cellular.Types where

import Control.Comonad

----------------------------------------------------------------------
---- Dimension stuff -------------------------------------------------
----------------------------------------------------------------------

data D n = D n | Up | Down

class DType d where
  empty :: d

instance DType () where
  empty = ()
  
instance (DType d) => DType (D d) where
  empty = D empty

-- demote :: (d ~ D n, DType d) => D d -> d -- requires typefamilies
demote :: (DType d) => D d -> d
demote (D n) = n
demote _ = empty

----------------------------------------------------------------------

data Cell c = Cell c
  deriving (Show, Eq, Ord)

data U u c = U [u c] (u c) [u c]
  deriving (Show, Eq, Ord)


-- Bijective Type Relation!  I totally didn't expect this to be
-- possible!
class Dimension (u :: * -> *) d | u d -> u d where
  shift :: (DType d) => d -> u c -> u c

instance Dimension Cell () where
  shift _ = id

instance (Dimension u d, DType d) => Dimension (U u) (D d) where
  shift d (U (a:as) x (b:bs)) = 
    case d of
    
      Up   -> U       as a (x:b:bs)
      Down -> U (x:a:as) b       bs
      
      _ -> let s = shift (demote d)
           in U (map s (a:as)) 
                (s x) 
                (map s (b:bs))


instance Functor Cell where
  fmap f (Cell c) = Cell (f c)

instance (Functor u) => Functor (U u) where
  fmap f (U as x bs) = U (map (fmap f) as) 
                         (fmap f x) 
                         (map (fmap f) bs)


instance Comonad Cell where
  extract (Cell x) = x
  duplicate c = Cell c
  
instance (Comonad u, Dimension u d, DType d) => Comonad (U u) where
  extract (U _ x _) = extract x
  duplicate u = U (dshift Up u)
                  (dupSlice u)
                  (dshift Down u)

dshift d = map dupSlice . tail . iterate (shift d)

dupSlice :: U u c -> u (U u c)
dupSlice = undefined

-- dupSlice :: Chessboard c -> SW.Sidewalk (Chessboard c)
-- dupSlice u = SW.mkSidewalk (tail $ iterate (shift West) u)
--                            u
--                            (tail $ iterate (shift East) u)

-- instance Comonad Sidewalk where
--   extract (Sidewalk _ x _) = x
--   duplicate u = Sidewalk (tail $ iterate (shift West) u) 
--                          u 
--                          (tail $ iterate (shift East) u)

----------------------------------------------------------------------
---- Universes and Cells ---------------------------------------------
----------------------------------------------------------------------

-- class Cell u c | c -> u where
--   stepCell :: u c -> c


-- class ToChar c where
--   toChar :: c -> Char
