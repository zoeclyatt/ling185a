{-# LANGUAGE FlexibleInstances #-}

module Assignment05 where

import Control.Applicative(liftA, liftA2, liftA3)

import SemiringFSA

data Numb = Z | S Numb deriving Show

distrib_lhs :: (Semiring v) => v -> v -> v -> v
distrib_lhs x y z = x &&& (y ||| z)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

dotprod :: (Semiring v) => [v] -> [v] -> v 
dotprod x y = case x of
              [] -> gfalse
              [thingx] -> case y of
                          [] -> gfalse
                          [thingy] -> thingx &&& thingy
                          heady:resty -> (thingx &&& heady)
              headx:restx -> case y of
                           [] -> gfalse
                           [thingy] -> headx &&& thingy
                           heady:resty -> (headx &&& heady) ||| (dotprod restx resty)

expn :: (Semiring v) => v -> Numb -> v
expn x n = case n of
           Z -> gtrue
           S y -> x &&& (expn x y)

backward :: (Semiring v) => GenericAutomaton st sy v -> [sy] -> st -> v
backward m w q = let (states, syms, i, f, delta) = m in
                 case w of
                 [] -> f q
                 x:rest -> gen_or (map (\q1 -> delta (q, x, q1) &&& backward m rest q1) states)

f :: (Semiring v) => GenericAutomaton st sy v -> [sy] -> v
f m w = let (states, syms, i, f, delta) = m in
        gen_or (map (\q0 -> i q0 &&& backward m w q0) states)

addCost :: Cost -> Cost -> Cost
addCost x y = case x of
              TheInt a -> case y of
                            TheInt b -> TheInt (a + b)
                            Inf -> Inf
              Inf -> Inf

minCost :: Cost -> Cost -> Cost
minCost x y = case x of
              TheInt a -> case y of
                          TheInt b -> TheInt (min a b)
                          Inf -> x
              Inf -> y

instance Semiring Cost where
   x &&& y = addCost x y
   x ||| y = minCost x y
   gtrue = TheInt 0
   gfalse = Inf

myConj :: [[a]] -> [[a]] -> [[a]]
myConj x y = case x of
             [] -> []
             [headx:tailx] -> case y of
                            [] -> []
                            [heady:taily] -> concat (map (\x1 -> map (\y1 -> x1 ++ y1) y) x)
                       
instance Semiring [[a]] where
   x &&& y = myConj x y
   x ||| y = x ++ y
   gtrue = [[]]
   gfalse = []
