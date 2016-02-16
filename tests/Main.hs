{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main (main) where

import Control.Comonad
import System.Exit
import Data.List (intersperse)

import Data.Cellular

main = do let init = vis uni2
              end = (vis (head (drop 30 (iterate next uni2))))
          putStrLn init
          putStrLn ""
          putStrLn end
          if end == step30
             then return ()
             else do putStrLn step30
                     die "Conway test did not match"

step30 = ".........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.................oooo....................\n\
          \........oo......oo...o...................\n\
          \.......o.o......o...oo...................\n\
          \......oo.o.......oo......................\n\
          \.......oo................................\n\
          \........o........ooo.....................\n\
          \................oo.......................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \.........................................\n\
          \........................................."

uni2 :: U2 Conway
uni2 = (set On . shift right . set On . shift left . shift down . set On . shift up . shift up . set On . shift left . set On . shift right . shift down) (uniform Off)

set c = modFocus (const c)

uni :: U2 FooCell
uni = modFocus (const FooLive) (uniform FooDead)

vis :: U2 Conway -> String
vis = concat . intersperse "\n"
      . map (map (\c -> case c of
                          C On -> 'o'
                          C Off -> '.')) 
      . map (mkList 20) 
      . (mkList 20)

data FooCell = FooLive | FooDead
  deriving (Show, Read, Eq, Ord)
  
type U0 = C

type U1 = U U0

type U2 = U U1

instance Automaton (U2) FooCell where
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

data Conway = On | Off
  deriving (Show, Read, Eq, Ord)
  
instance Automaton U2 Conway where
  rule = conway
  
conway :: U2 Conway -> Conway
conway u = let ns = [extract (shift right u)
                    ,extract (shift left u)
                    ,extract (shift up u)
                    ,extract (shift down u)
                    ,extract ((shift up . shift right) u)
                    ,extract ((shift up . shift left) u)
                    ,extract ((shift down . shift right) u)
                    ,extract ((shift down . shift left) u)]
               count = length (filter (== On) ns)
               self = extract u
           in case self of
                On -> if count < 2
                         then Off
                         else if count > 3
                                 then Off
                                 else On
                Off -> if count == 3
                          then On
                          else Off
