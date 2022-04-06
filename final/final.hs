-------------------------------------
-------------------------------------
------ LING 185A FINAL PROJECT ------
-------------------------------------
-------------------------------------

-------------------------------------
---------- types and stuff ----------
-------------------------------------

module Final where

-- from transition-based CFG parsing instructions
data StackSymbol nt = Plain nt | Barred nt deriving (Eq,Show)

-- from ContextFree.hs
data Cat = S | NP | VP | PP | D | N | V | P | WHILE | POSS | ORC | SRC | THAT deriving (Show, Eq, Ord)

-- from ContextFree.hs
data Rule nt t = NTRule nt [nt] | TRule nt t deriving (Show, Eq)

-- from transition-based CFG parsing instructions
type Configuration a b = ([a],[b])

-------------------------------------
------- stuff for test cases --------
-------------------------------------

-- rules for CFG in handout 7
rules :: [Rule Cat String]
rules = [ -----------------------
          -- nonterminal rules --
          -----------------------
          -- rules for S
             NTRule S [NP, VP], NTRule S [WHILE, S, S], 
          -- rules for NP
             NTRule NP [D, N], NTRule NP [D, N, PP],
             NTRule NP [D, N, PP, SRC], NTRule NP [D, N, PP, ORC],
             NTRule NP [D, N, PP, SRC, ORC], NTRule NP [D, N, SRC], 
             NTRule NP [D, N, SRC, ORC], NTRule NP [D, N, ORC],
             NTRule NP [N], NTRule NP [N, PP], 
             NTRule NP [N, PP, SRC], NTRule NP [N, PP, SRC, ORC],
             NTRule NP [N, PP, ORC], NTRule NP [N, SRC],
             NTRule NP [N, SRC, ORC], NTRule NP [N, ORC], 
             NTRule NP [NP, POSS, N],  
          -- rules for VP
             NTRule VP [V], NTRule VP [V, NP],
             NTRule VP [V, NP, PP], NTRule VP [V, PP],
          -- rule for PP
             NTRule PP [P, NP],
          -- rule for SRC
             NTRule SRC [THAT, VP],
          -- rule for ORC
             NTRule ORC [NP, V],
          --------------------
          -- terminal rules --
          --------------------
          -- rules for N
             TRule N "baby", TRule N "boy", TRule N "actor",
             TRule N "wife", TRule N "boss", TRule N "award",
          -- rules for NP
             TRule NP "Mary", TRule NP "John",
          -- rules for V
             TRule V "met", TRule V "saw", TRule V "won",
          -- rule for D
             TRule D "the",
          -- rules for P
             TRule P "on", TRule P "in", TRule P "with",
          -- rule for THAT
             TRule THAT "that",
          -- rule for POSS
             TRule POSS "'s",
          -- rule for WHILE
             TRule WHILE "while"
        ]

-- left-branching structures --
lba :: [[Char]]
lba = words "Mary won"

lbb :: [[Char]]
lbb = words "Mary 's baby won"

lbc :: [[Char]]
lbc = words "Mary 's boss 's baby won"

-- right-branching structures --
rba :: [[Char]]
rba = words "John met the boy"

rbb :: [[Char]]
rbb = words "John met the boy that saw the actor"

rbc :: [[Char]]
rbc = words "John met the boy that saw the actor that won the award"

-- center-embedding structures --
cea :: [[Char]]
cea = words "the actor won"

ceb :: [[Char]]
ceb = words "the actor the boy met won"

cec :: [[Char]]
cec = words "the actor the boy the baby saw met won"

-------------------------------------
------------- functions -------------
-------------------------------------

-- shift
shift :: (Eq nt) => (Eq t) => [Rule nt t] -> Configuration nt t -> [Configuration nt t]
shift r c = let (nts, ts) = c in
            case ts of
            [] -> (:[]) c
            h:t -> let thing = shiftHelper r h in
                   if thing == [] then []  else [(nts ++ thing, t)]

shiftHelper :: (Eq t) => [Rule nt t] -> t -> [nt]
shiftHelper r thing = case r of
                      [] -> []
                      h:t -> case h of
                             NTRule x y -> shiftHelper t thing
                             TRule x y -> if y == thing then (:[]) x else shiftHelper t thing

-- reduce
reduce :: (Eq nt) => (Eq t) => [Rule nt t] -> Configuration nt t -> [Configuration nt t]
reduce r c = let (nts, ts) = c in
             case nts of
             [] -> (:[]) c
             _ -> case r of
                  [] -> (:[]) c
                  h:t -> case h of
                         TRule x y -> reduce t c
                         NTRule x y -> let value = reduceHelper x y nts in
                                       case value of
                                       [] -> reduce t c
                                       _ -> (:[]) (value, ts) ++ reduce t c

reduceHelper :: (Eq nt) => nt -> [nt] -> [nt] -> [nt]
reduceHelper lhs rhs things = case rhs of
                             [] -> things ++ ((:[]) lhs)
                             _ -> case things of
                                  [] -> []
                                  _ -> if (last rhs) == (last things) then reduceHelper lhs (init rhs) (init things) else []

-- predict
predict :: (Eq nt) => [Rule nt t] -> Configuration nt t -> [Configuration nt t]
predict r c = let (nts, ts) = c in
              case nts of 
              [] -> (:[]) c
              _ -> case r of
                   [] -> (:[]) c
                   h:t -> case h of
                          TRule x y -> predict t c
                          NTRule x y -> let value = predictHelper x y nts in
                                        case value of
                                        [] -> predict t c
                                        _ -> (:[]) (value, ts) ++ predict t c

predictHelper :: (Eq nt) => nt -> [nt] -> [nt] -> [nt]
predictHelper lhs rhs things = if lhs == (head things) then rhs ++ (tail things) else []

-- match
match :: (Eq nt) => (Eq t) => [Rule nt t] -> Configuration nt t -> [Configuration nt t]
match r c = let (nts, ts) = c in
            case nts of
            [] -> (:[]) c
            hn:tn -> case r of
                     [] -> (:[]) c
                     hr:tr -> case hr of
                              NTRule x y -> match tr c
                              TRule x y -> if x == hn then if (head ts) == y then [(tn, (tail ts))] else match tr c else match tr c

-- lc-predict
lcPredict :: (Eq nt) => [Rule nt t] -> Configuration (StackSymbol nt) t -> [Configuration (StackSymbol nt) t]
lcPredict r c = let (nts, ts) = c in
                case nts of 
                [] -> (:[]) c
                hn:tn -> case hn of
                         Barred nont -> (:[]) c
                         Plain nont -> case r of
                                       [] -> (:[]) c
                                       hr:tr -> case hr of 
                                                TRule x y -> lcPredict tr c
                                                NTRule x y -> let value = lcPredictHelper x y nts in
                                                              case value of
                                                              [] -> lcPredict tr c
                                                              _ -> (:[]) (value, ts) ++ lcPredict tr c

lcPredictHelper :: (Eq nt) => nt -> [nt] -> [StackSymbol nt] -> [StackSymbol nt]
lcPredictHelper lhs rhs things = case things of
                                 [] -> []
                                 ht:tt -> case ht of
                                          Barred nont -> []
                                          Plain nont -> case rhs of 
                                                        [] -> []
                                                        hr:tr -> if hr == nont then (bar tr) ++ [Plain lhs] ++ tt else []

bar :: [nt] -> [StackSymbol nt]
bar x = case x of
        [] -> []
        h:t -> Barred h : bar t
                         
-- lc-connect
lcConnect :: (Eq nt) => [Rule nt t] -> Configuration (StackSymbol nt) t -> [Configuration (StackSymbol nt) t]
lcConnect r c = let (nts, ts) = c in
                case nts of
                [] -> (:[]) c
                hn:tn -> case hn of
                         Barred nont -> (:[]) c
                         Plain nont -> case r of
                                       [] -> (:[]) c
                                       hr:tr -> case hr of
                                                TRule x y -> lcConnect tr c
                                                NTRule x y -> let value = lcConnectHelper x y nts in
                                                              case value of
                                                              [] -> lcConnect tr c
                                                              _ -> (:[]) (value, ts) ++ lcConnect tr c

lcConnectHelper :: (Eq nt) => nt -> [nt] -> [StackSymbol nt] -> [StackSymbol nt]
lcConnectHelper lhs rhs things = case things of
                                 [] -> []
                                 ht:tt -> case ht of
                                          Barred nont -> []
                                          Plain nont -> let next = head tt in
                                                        case next of 
                                                        Plain nonterm -> []
                                                        Barred nonterm -> case rhs of
                                                                          [] -> []
                                                                          hr:tr -> if lhs == nonterm then if hr == nont then (bar tr) ++ (tail tt) else [] else [] 

-- lc-shift (same thing as shift but for left-corner parsing)
lcShift :: (Eq nt) => (Eq t) => [Rule nt t] -> Configuration (StackSymbol nt) t -> [Configuration (StackSymbol nt) t]
lcShift r c = let (nts, ts) = c in
            case ts of
            [] -> (:[]) c
            h:t -> let thing = lcShiftHelper r h in
                   if thing == [] then [] else [(thing ++ nts, t)]

lcShiftHelper :: (Eq t) => [Rule nt t] -> t -> [StackSymbol nt]
lcShiftHelper r thing = case r of
                      [] -> []
                      h:t -> case h of
                             NTRule x y -> lcShiftHelper t thing
                             TRule x y -> if y == thing then (Plain x) : [] else lcShiftHelper t thing

-- lc-match (same thing as match but for left-corner parsing)
lcMatch :: (Eq nt) => (Eq t) => [Rule nt t] -> Configuration (StackSymbol nt) t -> [Configuration (StackSymbol nt) t]
lcMatch r c = let (nts, ts) = c in
            case nts of
            [] -> (:[]) c
            hn:tn -> case hn of
                     Plain nont -> (:[]) c
                     Barred nont -> case r of
                                    [] -> (:[]) c
                                    hr:tr -> case hr of
                                             NTRule x y -> lcMatch tr c
                                             TRule x y -> if x == nont then if (head ts) == y then [(tn, (tail ts))] else lcMatch tr c else lcMatch tr c

