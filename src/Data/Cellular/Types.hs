{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Cellular.Types where

import Control.Comonad

----------------------------------------------------------------------
---- Dimension stuff -------------------------------------------------
----------------------------------------------------------------------



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

data C c = C c
  deriving (Show, Eq, Ord)

instance Functor C where
  fmap f (C c) = C (f c)

instance Comonad C where
  extract (C x) = x
  duplicate c = C c

data U u c = U [u c] (u c) [u c]
  deriving (Show, Eq, Ord)

shiftUp, shiftDown :: U u c -> U u c
shiftUp   (U (a:as) x     bs) = U     as a (x:bs)
shiftDown (U     as x (b:bs)) = U (x:as) b     bs

instance (Functor u) => Functor (U u) where
  fmap f (U as x bs) = U (map (fmap f) as) 
                         (fmap f x) 
                         (map (fmap f) bs)

instance (Comonad u) => Comonad (U u) where
  extract (U _ x _) = extract x
  duplicate u = U (dshift shiftUp u)
                  (dupSlice u)
                  (dshift shiftDown u)

dshift s = map dupSlice . tail . iterate s

dupSlice :: (Comonad u) => U u c -> u (U u c)
dupSlice u@(U _ x _) = fmap (const u) x

-- Bijective Type Relation!  I totally didn't expect this to be
-- possible!
class Dimension (u :: * -> *) d | u d -> u d where
  shift :: (DType d) => d -> u c -> u c

data D n = D n | Up | Down

instance Dimension C () where
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
