{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Cellular.Automaton.Conway
  ( Conway (Life, Death)
  
  , conwayChar
  
  ) where

import Data.Cellular
import Data.Cellular.Universe.Chessboard

data Conway = Life | Death deriving Eq

instance Automaton Chessboard Conway where
  rule = conway
  
neighbors = adjacent ++ diagonal

living :: Chessboard Conway -> [Path Chessboard] -> Int
living u = length . filter (== Life) . map ((flip look) u)

conway :: Chessboard Conway -> Conway
conway u = case look here u of
             Life -> if alive < 2
                        then Death
                        else if alive > 3
                                then Death
                                else Life
             Death -> if alive == 3
                         then Life
                         else Death
  where alive = living u neighbors

conwayChar :: Conway -> Char
conwayChar Life = 'o'
conwayChar _ = '.'
