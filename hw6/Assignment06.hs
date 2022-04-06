module Assignment06 where

import Control.Applicative(liftA, liftA2, liftA3)

import ContextFree

import qualified Memoization as M

data Tree nt t = Leaf nt t | NonLeaf nt (Tree nt t) (Tree nt t) deriving Show

tree1 :: Tree Cat String
tree1 = NonLeaf VP (NonLeaf VP (Leaf V "watches") (Leaf NP "spies"))
                   (NonLeaf PP (Leaf P "with") (Leaf NP "telescopes"))

tree2 :: Tree Cat String
tree2 = NonLeaf VP (Leaf V "watches")
                   (NonLeaf NP (Leaf NP "spies") (NonLeaf PP (Leaf P "with") (Leaf NP "telescopes")))

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

