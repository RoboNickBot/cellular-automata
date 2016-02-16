{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main (main) where

import Control.Comonad
import System.Exit
import Data.List (intersperse)

import Data.Cellular

main = do putStrLn (vis uni)
          putStrLn ""
          putStrLn (vis (next uni))
          putStrLn ""
          putStrLn (vis ((next . next) uni))
          putStrLn ""
          putStrLn (vis ((next . next . next) uni))
          die "test"


uni :: UStack U2 FooCell
uni = UStack (modFocus (const FooLive) (uniform FooDead))

vis :: UStack U2 FooCell -> String
vis (UStack u) = go u
  where go = concat . intersperse "\n"
             . map (map (\c -> case c of
                                 C FooLive -> 'o'
                                 C FooDead -> '.')) 
             . map (mkList 5) 
             . (mkList 5)

data FooCell = FooLive | FooDead
  deriving (Show, Read, Eq, Ord)
  
type U1 = U C

type U2 = U (U C)

instance Automaton (UStack U2) FooCell where
  rule = rule3

rule1 u = extract u

rule2 u = extract ((shift right) u)

rule3 u = case extract (shift right u) of
            FooLive -> FooLive
            _ -> case extract ((shift down . shift right) u) of
                   FooLive -> FooLive
                   _ -> FooDead

left = Stack Up :: DStack U2

right = Stack Down :: DStack U2

up = Down :: DStack U2

down = Up :: DStack U2
