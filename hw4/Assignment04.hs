module Assignment04 where

import Prelude hiding (Either(..))

import Control.Applicative(liftA, liftA2, liftA3)
import Data.List(nub)

import FiniteStatePart2

---------------------------------------
-- Setup for section 1

type SLG sy = ([sy], [sy], [sy], [(sy,sy)])
data ConstructedState sy = ExtraState | StateForSymbol sy deriving (Eq, Show)

slg1 :: SLG SegmentCV
slg1 = ([C,V], [C], [V], [(C,C),(C,V),(V,V)])

slg2 :: SLG Int
slg2 = ([1,2,3], [1,2,3], [1,2,3], [(1,1),(2,2),(3,3),(1,2),(2,1),(1,3),(3,1)])

---------------------------------------
-- Setup for section 2

data Either a b = First a | Second b deriving (Show,Eq)

re1 :: RegExp Char
re1 = Concat (Alt (Lit 'a') (Lit 'b')) (Lit 'c')

re2 :: RegExp Char
re2 = Star re1

re3 :: RegExp Int
re3 = Star (Concat ZeroRE (Lit 3))

re4 :: RegExp Int
re4 = Concat (Alt (Lit 0) (Lit 1)) (Star (Lit 2))

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

backwardSLG :: (Eq sy) => SLG sy -> [sy] -> Bool 
backwardSLG m w = let (syms, i, f, delta) = m in
                    case w of
                    [x] -> elem x f                                                                                                                                                                     
                    (x:rest) -> elem (x, (head rest)) delta && backwardSLG m rest

generatesSLG :: (Eq sy) => SLG sy -> [sy] -> Bool
generatesSLG m w = let (syms, i, f, delta) = m in
                   case w of
                   [] -> False
                   x -> elem (head w) i && backwardSLG m w

--honestly this just started taking way longer than i thought it would and i ran out of time... sorry :(
--makeTuples states oldI oldF oldDelta = 

--slgToFSA :: SLG sy -> Automaton (ConstructedState sy) sy
--slgToFSA m = let (syms, i1, f1, delta1) = m in
--             let states = (map (\x -> x) [0 .. (length syms)]) in
--             let i2 = ... in
--             let f2 = ... in
--             let delta2 = ... in
--             (states, syms, i2, f2, delta2)
