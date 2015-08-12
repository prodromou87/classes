{- CSE 105 FA 14: HW1 P3
 - Name: Andreas Prodromou
 - E-mail: aprodrom@eng.ucsd.edu
 - ID: A53049230
 - Collaborated with: NONE
 - 
 -}


module Main where

import DFA
import Numeric 
import Data.Char
import Data.List

{-
 -
 - A few more useful list functions and constructs.
 -
 -
 - elem :: Eq a => a -> [a] -> Bool
 -
 - Evaluates to True if the element that is its first argument is an
 - element of the list that is its second argument, and to False
 - otherwise.  Elements of the list must be comparable for equality.
 - Elem is usually written, with backquotes, as an infix operator.
 -
 - Example: elem 7 [1, 3, 7, 11]   =>   True
 - Example: 7 `elem` [1, 3, 7, 11]   =>   True
 -
 -
 - filter :: (a -> Bool) -> [a] -> [a]
 - 
 - Given a predicate and a list, returns a list with only those elements
 - of the original list on which the predicate evaluates to True.
 -
 - Example:  filter (>5) [1,4,7,6]   ==>   [7,6]
 -
 -
 - List comprehensions:
 -     [ expr | var1 <- list1, var2 <- list2, ..., cond1, cond2, ... ]
 -
 - Let var1 range over all elements of list1, var2 over all elements of
 - list2, and so on; for each possible assignment values to of var1,
 - var2, and so on, evaluate cond1, cond2, and so on.  If all of these
 - conditions evaluate to True (or if there are no conditions listed),
 - evaluate expr.  The result is a list of all evaluated exprs.
 -
 - Example: [ toUpper c | c <- "Hello" ]   =>   "HELLO"
 - Example: [ x+y | x <- [0,10,100], y <- [1,2,3] ]
 -             =>   [1,2,3,11,12,13,101,102,103]
 - Example: [ sqrt x | x <- [ -2, 2, -1, 1, 0] ]
 -             =>   [NaN,1.4142135623730951,NaN,1.0,0.0]
 - Example: [ sqrt x | x <- [ -2, 2, -1, 1, 0], x >= 0 ]
 -             =>   [1.4142135623730951,1.0,0.0]
 - 
 - 
 - foldl :: (b -> a -> b) -> b -> [a] -> b
 -
 - foldl takes three arguments.  The third of these arguments is a list
 - that it will "fold" from left to right (hence its name) into a
 - single value.  The fold will proceed one element at a time, using
 - the function that is foldl's first argument.  This function will take
 - the result so far and the leftmost remaining element of the list and
 - combine them into an updated result-so-far.  To get started, foldl
 - also takes, as its second argument, the value to start with as the
 - result-so-far.  Note the order of arguments for the function that is
 - foldl's first argument, and note that the type of the input list and
 - the type of the result can differ.
 -
 - Example: foldl (+) 0 [1..10]   =>   55
 - Example: foldl (\x y -> y:x) "" "Hello"   =>   "olleH"
 -
 -}

m_qs :: [Int]
m_qs        = [0, 1, 2, 3, 4, 5, 6, 7, 8]

m_sigma :: [Char]
m_sigma     = [ '0', '1', '2' ]

m_delta :: Int -> Char -> Int
m_delta 0 '0' = 1
m_delta 0 _ = 5
m_delta 1 '1' = 2
m_delta 1 _ = 5
m_delta 2 '2' = 3
m_delta 2 _ = 2
m_delta 3 _ = 4
m_delta 4 _ = 4
m_delta 5 _ = 6
m_delta 6 _ = 7
m_delta 7 _ = 8
m_delta 8 _ = 8
m_delta _ _ = error "Unknown State"

m_s :: Int
m_s         = 0

m_inF :: Int -> Bool
m_inF 4     = True
m_inF _     = False

machine :: DFA Int
machine = (m_qs, m_sigma, m_delta, m_s, m_inF)


-- powerset of a set
powerset :: [a] -> [[a]]
powerset [] = [ [] ]
powerset (x:xs) = (powerset xs) ++ map (x:) (powerset xs)

-- transitive closure of f starting from set s
closure :: Eq a => (a -> [a]) -> [a] -> [a]
closure f s = closure_uniq f (nub s)

-- unlike closure, above, assumes s does not include duplicates
closure_uniq f s = if (length new_s) == (length s) then s
                                                   else closure_uniq f new_s
    where new_s = nub (s ++ concatMap f s)                                            


reachable_states :: Eq st => DFA st -> [st]
reachable_states (qs, sigma, delta, s, inF) =
    closure (\q -> map (delta q) sigma) [s]

live_states :: Eq st => DFA st -> [st]
live_states (qs, sigma, delta, s, inF) =
    closure hasnext (filter inF qs) where
        hasnext q = [r | r <- qs, x <- sigma, q == delta r x]

{-
 - NFA and NFAe datatypes, in case you need them.
 -
- NFAe is an NFA with epsilon transitions (what Sipser just calls an
 - NFA).  Following Prof. Micciancio, we use the convention that the
 - convention that the transition function delta(q,x) is defined for
 - x::(Maybe Char), where "Just a" stands for the character a and
 - "Nothing" stands for an epsilon transition.
 -
 - NFA is an NFA *without* epsilon transitions.  As with DFAs, an NFA's
 - delta function takes x::Char, not x::(Maybe Char).
 -}

type NFAe st = ([st], [Char], st->(Maybe Char)->[st], st, st->Bool)
type NFA  st = ([st], [Char], st->Char->[st],         st, st->Bool)


{-
 - Conversion routines, in case you need them.
 -}

conv_NFAe_to_NFA :: Eq st => NFAe st -> NFA st

conv_NFAe_to_NFA (qs, sigma, delta, s, inF) =
    (qs, sigma, delta', s, inF') where
        delta' q x = nub (concatMap (\r -> delta r (Just x)) (eclose q))
        inF' q = any inF (eclose q)
        -- eclose q is what sipser calls E(q)
        eclose q = closure (\r -> delta r Nothing) [q]

-- note the change in state type-variable in the type here:
conv_NFA_to_DFA :: Eq st => NFA st -> DFA [st]

conv_NFA_to_DFA (qs, sigma, delta, s, inF) =
    (qs', sigma, delta', s', inF') where
        qs' = powerset qs
        delta' qset x = nub (concatMap (\q -> delta q x) qset)
        s' = [ s ]
        inF' qset = any inF qset


{- BEGIN PART TO FILL IN, PROBLEM 1 -}

threes :: Eq st => DFA st -> DFA (st, Int)
threes (qs, sigma, delta, s, inF) =
    (qs', sigma, delta', s', inF') where
        qs' = [(q,x) | q <- qs, x <- [0,1,2,3,4]]
	inF' (q,3) = elem q (live_states (qs, sigma, delta, s, inF))
	inF' _ = False
	delta' (q,4) c = (delta q c, 4)
	delta' (q,x) c = (delta q c, x+1)
	s' = (s,0)

{- END PART TO FILL IN, PROBLEM 1 -}


{- BEGIN PART TO FILL IN, PROBLEM 2 -}

-- the "before/after" datatype from class, in case you need it

mm_qs :: [Int]
mm_qs        = [0, 1, 2, 3, 4]

mm_sigma :: [Char]
mm_sigma     = [ '0', '1']

mm_delta :: Int -> Char -> Int
mm_delta 0 '0' = 1
mm_delta 0 _ = 4
mm_delta 1 '1' = 2
mm_delta 1 _ = 4
mm_delta 2 '0' = 3
mm_delta 2 '1' = 4
mm_delta 3 _ = 4
mm_delta 4 _ = 4
mm_delta _ _ = error "Unknown State"

mm_s :: Int
mm_s         = 0

mm_inF :: Int -> Bool
mm_inF 2     = True
mm_inF 3     = True
mm_inF _     = False

mmachine :: DFA Int
mmachine = (mm_qs, mm_sigma, mm_delta, mm_s, mm_inF)

data BA a = Before a | After a deriving (Eq)

hasprefix :: DFA st -> NFA (BA st)
hasprefix (qs, sigma, delta, s, inF) =
    (qs', sigma, delta', s', inF') where
	qs' = [(Before q) | q <- qs] ++ [(After q) | q <- qs]
	s' = (Before s)
	inF' (Before q) = False
	inF' (After q) = inF q
	delta' (Before q) x = if inF q 
			      then [After (delta q x)]
			      else [Before (delta q x)]
	delta' (After q) x = [After (delta q x)]

{- END PART TO FILL IN, PROBLEM 2 -}
