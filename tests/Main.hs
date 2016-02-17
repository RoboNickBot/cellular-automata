{-# LANGUAGE RankNTypes #-}
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
             else do putStrLn ""
                     putStrLn step30
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
uni2 = (extend up . poke down . extend right . poke left . poke down . poke left . poke up)
         (uniform Off)
         
uni2 :: U2 Conway
poke = set On

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


left = dir (Stack Up) :: Dir U2

right = dir (Stack Down) :: Dir U2

up = dir Up :: Dir U2

down = dir Down :: Dir U2

self = dir (Stack (Stack Base)) :: Dir U2

data Conway = On | Off
  deriving (Show, Read, Eq, Ord)
  
instance Automaton U2 Conway where
  rule = conway
  
dcat :: Dir u -> Dir u -> Dir u
dcat d1 d2 u = d1 . extend d2 $ u

conway :: U2 Conway -> Conway
conway u = let ns = [right u
                    ,left u
                    ,up u
                    ,down u
                    ,up (extend right) u
                    ,up (extend left u)
                    ,down (extend right u)
                    ,down (extend left u)]
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
