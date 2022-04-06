module Assignment08 where

import Control.Applicative(liftA, liftA2, liftA3)

import TreeGrammars

plainWords = ["John","Mary","ate","bought","an","apple","books","yesterday","C","laughed","because"]
whWords = ["who","what","why"]
qWords = ["Q"]

------------------------------------------------------
-- Some tiny helpers for writing trees more compactly

-- ``leaf''
lf :: a -> Tree a
lf x = Node x []

-- ``merge''
mrg :: Tree String -> Tree String -> Tree String
mrg t1 t2 = Node "*" [t1,t2]

------------------------------------------------------

-- (1a)/(2a) `C John ate an apple yesterday'
tree_1a :: Tree String
tree_1a = mrg (lf "C") (mrg (lf "John") (mrg (mrg (lf "ate") (mrg (lf "an") (lf "apple"))) (lf "yesterday")))

-- (1b)/(2b) `Q John ate what yesterday'
tree_1b :: Tree String
tree_1b = mrg (lf "Q") (mrg (lf "John") (mrg (mrg (lf "ate") (lf "what")) (lf "yesterday")))

-- (3a) `Q John ate an apple yesterday'
tree_3a :: Tree String
tree_3a = mrg (lf "Q") (mrg (lf "John") (mrg (mrg (lf "ate") (mrg (lf "an") (lf "apple"))) (lf "yesterday")))

-- (3b) `C John ate what yesterday'
tree_3b :: Tree String
tree_3b = mrg (lf "C") (mrg (lf "John") (mrg (mrg (lf "ate") (lf "what")) (lf "yesterday")))

tree_13 :: Tree String
tree_13 =
    Node "*" [
        Node "Q" [],
        Node "*" [
            Node "John" [],
            Node "*" [
                Node "laughed" [],
                Node "**" [
                    Node "because" [],
                    Node "*" [
                        Node "Mary" [],
                        Node "*" [
                            Node "*" [Node "bought" [], Node "books" []],
                            Node "why" []
                        ]
                    ]
                ]
            ]
        ]
    ]

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

count :: (Eq a) => a -> Tree a -> Int
count sym tree = let Node x daughters = tree in
                 if x == sym then (1 + sum (map (\y -> count sym y) daughters)) else sum (map (\y -> count sym y) daughters)

leftEdge :: Tree a -> [a]
leftEdge tree = case tree of
                Node x [] -> [x]
                Node x (head:tail) -> x : leftEdge head

allLists :: Int -> [a] -> [[a]]
allLists i syms = case i of
                  0 -> [[]]
                  1 -> map (:[]) syms
                  _ -> case syms of
                       [] -> [[]]
                       _ -> concatMap (\x -> map (x ++) (allLists 1 syms)) $ allLists (i - 1) syms 

under :: (Eq st, Eq sy) => Automaton st sy -> Tree sy -> st -> Bool
under m w q = let (states, syms, f, delta) = m in
                 let Node x daughters = w in
                    case daughters of
                    [] -> elem ([], x, q) delta
                    _ -> or (map (\qs -> elem (qs, x, q) delta && underHelper m daughters qs) (allLists (length daughters) states))

underHelper :: (Eq st, Eq sy) => Automaton st sy -> [Tree sy] -> [st] -> Bool
underHelper m ws qs = case ws of
                      [] -> True
                      [w] -> under m w (head qs)
                      headw:tailw -> case qs of
                                     [] -> True
                                     headq:tailq -> under m headw headq && underHelper m tailw tailq

generates :: (Eq st, Eq sy) => Automaton st sy -> Tree sy -> Bool
generates m w = let (states, syms, f, delta) = m in
                   or (map (\q0 -> elem q0 f && under m w q0) states)

fsta_wh1 :: Automaton Int String
fsta_wh1 = ([0, 1, 2, 3], plainWords ++ whWords ++ qWords ++ ["*"], [0], 
              [([0, 0], "*", 0), ([0, 1], "*", 1), ([2, 0], "*", 3), ([3, 0], "*", 3), 
               ([1, 1], "*", 1), ([2, 1], "*", 0), ([3, 1], "*", 3),
               ([2, 2], "*", 3), ([3, 2], "*", 3),
               ([3, 3], "*", 3),
               ([], "Q", 2)
              ] 
              ++ map (\x -> ([], x, 1)) whWords 
              ++ map (\x -> ([], x, 0)) plainWords
           )

fsta_wh2 :: Automaton Int String
fsta_wh2 = ([0, 1, 2, 3], plainWords ++ whWords ++ qWords ++ ["*", "**"], [0], 
              [([0, 0], "*", 0), ([0, 0], "**", 0), ([0, 1], "*", 1), ([0, 1], "**", 3), 
               ([2, 0], "*", 3), ([2, 0], "**", 3), ([3, 0], "*", 3), ([3, 0], "**", 3), 
               ([1, 1], "*", 1), ([1, 1], "**", 3), ([2, 1], "*", 0), ([2, 1], "**", 0), 
               ([3, 1], "*", 3), ([3, 1], "**", 3), ([2, 2], "*", 3), ([2, 2], "**", 3), 
               ([2, 3], "*", 3), ([2, 3], "**", 3), ([3, 3], "*", 3), ([3, 3], "**", 3), 
               ([], "Q", 2)
              ]
              ++ map (\x -> ([], x, 1)) whWords
              ++ map (\x -> ([], x, 0)) plainWords
           )
















                    
                     
