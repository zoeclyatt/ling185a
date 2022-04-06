module Assignment03 where

-- Imports everything from the FiniteState module
import FiniteState

-- Another type we'll use as symbols for some FSAs
data SegmentPKIU = P | K | I | U | MB deriving (Show,Eq)

-- A list-like type that will be useful for computing forward values
data SnocList a = ESL | (SnocList a) ::: a deriving Show

-- The word ``hello'' encoded as a snoc list of characters
sl :: SnocList Char
sl = ((((ESL ::: 'h') ::: 'e') ::: 'l') ::: 'l') ::: 'o'

-- Checks that all states and symbols mentioned in the transition 
-- table (i.e. delta) come from the provided lists of states and symbols.
fsaSanityCheck :: (Eq a) => Automaton a -> Bool
fsaSanityCheck m =
    let (states, syms, i, f, delta) = m in
    let validTransition (q1,x,q2) = elem q1 states && elem x syms && elem q2 states in
    and (map validTransition delta)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

fsa_countVs :: Automaton SegmentCV
fsa_countVs = ([54, 73, 21, 38], [C, V], [54], [38], [(54, C, 54),
                                                      (54, V, 73), 
                                                      (73, C, 73), 
                                                      (73, V, 21), 
                                                      (21, C, 21), 
                                                      (21, V, 54), 
                                                      (21, V, 38), 
                                                      (38, C, 38)])

addToFront :: a -> SnocList a -> SnocList a
addToFront x l = case l of
                 ESL -> (ESL ::: x)
                 a ::: b -> (addToFront x a) ::: b

toSnoc :: [a] -> SnocList a
toSnoc l = case l of
           [] -> ESL
           x:rest -> addToFront x (toSnoc (tail l))

forward :: (Eq a) => Automaton a -> SnocList a -> State -> Bool
forward m w q = let (states, syms, i, f, delta) = m in
                case w of
                ESL -> elem q i
                a ::: b -> or (map (\x -> elem (x, b, q) delta && forward m a x) states)

generates2 :: (Eq a) => Automaton a -> [a] -> Bool
generates2 m w = let (states,syms,i,f,delta) = m in
                or (map (\q0 -> elem q0 f && forward m (toSnoc w) q0) states)

fsa_twoCs :: Automaton SegmentCV
fsa_twoCs = ([0, 1, 2] , [C, V], [0], [2], [(0, V, 0), 
                                            (0, C, 1), 
                                            (1, V, 1), 
                                            (1, C, 2), 
                                            (2, V, 2), 
                                            (2, C, 2)])

fsa_thirdC :: Automaton SegmentCV
fsa_thirdC = ([0, 1, 2, 3, 4, 5, 6, 7], [C, V], [0], [7], [(0, V, 1), 
                                                           (0, C, 2), 
                                                           (1, V, 3), 
                                                           (1, C, 4), 
                                                           (2, V, 5), 
                                                           (2, C, 6), 
                                                           (3, C, 7), 
                                                           (4, C, 7), 
                                                           (5, C, 7), 
                                                           (6, C, 7), 
                                                           (7, V, 7), 
                                                           (7, C, 7)])

fsa_thirdlastC :: Automaton SegmentCV
fsa_thirdlastC = ([0, 1, 2, 3], [C, V], [0], [3], [(0, V, 0), 
                                                   (0, C, 0), 
                                                   (0, C, 1), 
                                                   (1, V, 2), 
                                                   (1, C, 2), 
                                                   (2, V, 3), 
                                                   (2, C, 3)])

fsa_oddEven :: Automaton SegmentCV
fsa_oddEven = ([0, 1, 2, 3], [C, V], [0], [3], [(0, V, 1), 
                                                (1, V, 0), 
                                                (1, C, 2), 
                                                (2, C, 1), 
                                                (2, V, 3), 
                                                (3, V, 2), 
                                                (3, C, 0), 
                                                (0, C, 3)])

fsa_harmony :: Automaton SegmentPKIU
fsa_harmony = ([0, 1, 2], [P, K, I, U, MB], [0], [0, 1, 2], [(0, P, 0), 
                                                             (0, K, 0), 
                                                             (0, MB, 0), 
                                                             (0, I, 1), 
                                                             (1, P, 1), 
                                                             (1, K, 1), 
                                                             (1, I, 1), 
                                                             (1, MB, 0), 
                                                             (0, U, 2), 
                                                             (2, P, 2), 
                                                             (2, K, 2), 
                                                             (2, U, 2), 
                                                             (2, MB, 0)])

fsa_MBU :: Automaton SegmentPKIU
fsa_MBU = ([0, 1], [P, K, I, U, MB], [0], [0, 1], [(0, P, 0),
                                                   (0, K, 0),
                                                   (0, I, 0),
                                                   (0, MB, 0),
                                                   (0, MB, 1),
                                                   (1, U, 1),
                                                   (1, P, 1),
                                                   (1, K, 1),
                                                   (1, I, 1),
                                                   (1, MB, 1)])

fsa_adjacentMBU :: Automaton SegmentPKIU
fsa_adjacentMBU = ([0, 1], [P, K, I, U, MB], [0], [0], [(0, P, 0), 
                                                       (0, K, 0), 
                                                       (0, I, 0),
                                                       (0, MB, 0), 
                                                       (0, MB, 1), 
                                                       (1, U, 0), 
                                                       (1, P, 0), 
                                                       (1, K, 0), 
                                                       (1, I, 0), 
                                                       (1, MB, 0)])

requireCs :: Int -> Automaton SegmentCV
requireCs n = let states = (map (\x -> x) [0 .. n]) in
              let syms = [C,V] in
              let i = [0] in
              let f = [n] in
              let ctransitions = (map (\x -> (x, C, (x + 1))) [0 .. (n - 1)]) in
              let vtransitions = (map (\x -> (x, V, x)) states) in
              (states, syms, i, f, ctransitions ++ vtransitions)
