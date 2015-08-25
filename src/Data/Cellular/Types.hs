{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Cellular.Types where

import Control.Comonad

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

----------------------------------------------------------------------

-- data D n = D n | Up | Down

data family Direction u
data instance Direction (C c) = Base
data instance Direction (U u c) = D (Direction (u c)) | Up | Down

class Universe u where
  empty :: Direction (u c)
  demote :: Direction (U u c) -> Direction (u c)
  shift :: Direction (u c) -> u c -> u c

instance Universe C where
  empty = Base
  demote _ = Base
  shift _ = id

instance (Universe u) => Universe (U u) where
  empty = D empty
  
  demote (D d) = d
  demote _ = empty

  shift Up u = shiftUp u
  shift Down u = shiftDown u
  shift d (U as x bs) = let s = shift (demote d)
                        in U (map s as)
                             (s x)
                             (map s bs)

-- Bijective Type Relation!  I totally didn't expect this to be
-- possible!  Of course, it didn't really work out.  Shoulda trusted
-- my instincts :(
-- class Dimension (u :: * -> *) d | u d -> u d where
--   empty :: d
--   demote :: D d -> d
--   shift :: d -> u c -> u c

-- instance Dimension C () where
--   empty = ()
--   demote _ = ()
--   shift _ = id

-- instance forall u d. (Dimension u d) => Dimension (U u) (D d) where

--   empty = D (empty :: d)

--   demote (D d) = d
--   demote _ = (empty :: d)

                             

-- class DType d where
--   empty :: d

-- instance DType () where
--   empty = ()

-- instance (DType d) => DType (D d) where
--   empty = D empty

-- -- demote :: (d ~ D n, DType d) => D d -> d -- requires typefamilies
-- demote :: (DType d) => D d -> d
-- demote (D n) = n
-- demote _ = empty

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
