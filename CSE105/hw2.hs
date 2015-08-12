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

m_qs :: [Int]
m_qs        = [0, 1, 2, 3, 4]

m_sigma :: [Char]
m_sigma     = [ '0', '1' ]

m_delta :: Int -> Char -> Int
m_delta 0 '0' = 1
m_delta 0 '1' = 1
m_delta 1 '0' = 2
m_delta 1 '1' = 2
m_delta 2 '0' = 0
m_delta 2 '1' = 0
m_delta 3 '0' = 2
m_delta 3 '1' = 3
m_delta 4 '0' = 3
m_delta 4 '1' = 3

m_delta _ _ = error "Unknown State"

m_s :: Int
m_s         = 0

m_inF :: Int -> Bool
m_inF 1     = True
m_inF _     = False

machine :: DFA Int
machine = (m_qs, m_sigma, m_delta, m_s, m_inF)

{-
 - Useful list functions.
 -
 - All but two of these -- powerset and closure -- are defined in
 - Data.List.
 -
 -
 - any :: (a -> Bool) -> [a] -> Bool
 - 
 - Given a predicate and a list, returns True if the predicate
 - evaluates to True on any element of the list, False otherwise.
 -
 - Example:  any (>5) [1,4,6]   ==>   True
 -
 -
 - map :: (a -> b) -> [a] -> [b]
 - 
 - Applies the function given as map's first argument to each element
 - of the list given as map's second argument.  Returns a list
 - consisting of the return values of the function on each element of
 - the list.  Note that if the function returns a list then map will
 - return a list of lists.
 -
 - Example: map (+1) [1,4,6]   ==>   [2,5,7]
 - Example: map (\x -> [x,x+1]) [1,4,6]   ==>   [[1,2],[4,5],[6,7]]
 - 
 -
 - concatMap :: (a -> [b]) -> [a] -> [b]
 -
 - concatMap expects the argument given as its first argument to
 - return a list.  It will apply this function to each element of the
 - list that is concatMap's second argument; it will then concatenate
 - all the resulting lists together.
 -
 - Example: concatMap (\x -> [x,x+1]) [1,4,6]   ==>   [1,2,4,5,6,7]
 -
 -
 - nub :: Eq a => [a] -> [a]
 -
 - nub takes a list of values and returns a list with those same
 - values, but with any duplicates removed.  Since the notion of
 - "duplicates" requires a notion of equality of values, nub applies
 - only to types a in the "Eq" typeclass, i.e., types whose values can
 - be compared.  You should not run into any values not in this
 - typeclass.
 -
 - Example: nub [3,1,4,1,5,9,2,6,5,3,5]   ==>   [3,1,4,5,9,2,6]
 -
 -
 - powerset :: [a] -> [[a]]
 -
 - Returns the powerset of the set given as its argument.
 -
 - Example: powerset [1,2,3]
 -           ==>   [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]
 -
 -
 - closure :: Eq a => (a -> [a]) -> [a] -> [a]
 -
 - Takes, as its first argument, a function that, given a value of
 - some type, returns a list of values of the same type.  Returns the
 - transitive closure of this function when starting from the list of
 - values that is closure's second argument.
 -
 - (The transitive closure of a function f on a set S is the smallest
 - set X that contains S such that f(x) \subseteq X for all x \in X.)
 -
 - Example: closure (\x -> if x < 16 then [x*2,x*3] else []) [1,3,7]
 -          ==>   [1,3,7,2,6,9,14,21,4,12,18,27,28,42,8,24,36,16]
 -
 -}


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



{- BEGIN PART TO FILL IN, PROBLEM 2 -}
{- returns all the neighbors of a state q over all posible chars -}

next :: (st -> Char -> st) -> [Char] -> st -> [st]
next delta sigma q = map (delta q) sigma

reachable_states :: Eq st => DFA st -> [st]
reachable_states (qs, sigma, delta, s, inF) =
    closure (next delta sigma) [s]

{- END PART TO FILL IN, PROBLEM 2 -}



{-
 - NFA and NFAe datatypes.
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

x_qs :: [Int]
x_qs        = [0, 1, 2]

x_sigma :: [Char]
x_sigma     = [ '0', '1' ]

x_delta :: Int -> Char -> [Int]
x_delta 0 '0' = [1]
x_delta 0 '1' = [2]
x_delta 1 '0' = []
x_delta 1 '1' = [2]
x_delta 2 '0' = [1]
x_delta 2 '1' = [1]

x_delta _ _ = error "Unknown State"

x_s :: Int
x_s         = 0

x_inF :: Int -> Bool
x_inF 2     = True
x_inF _     = False

n_machine :: NFA Int
n_machine = (x_qs, x_sigma, x_delta, x_s, x_inF)

{-
 - Function that converts an NFAe to an NFA.  This is provided for
 - your reference.
 -}

conv_NFAe_to_NFA :: Eq st => NFAe st -> NFA st

conv_NFAe_to_NFA (qs, sigma, delta, s, inF) =
    (qs, sigma, delta', s, inF') where
        delta' q x = nub (concatMap (\r -> delta r (Just x)) (eclose q))
        inF' q = any inF (eclose q)
        -- eclose q is what sipser calls E(q)
        eclose q = closure (\r -> delta r Nothing) [q]


{- BEGIN PART TO FILL IN, PROBLEM 3 -}

-- note the change in state type-variable in the type here:
conv_NFA_to_DFA :: Eq st => NFA st -> DFA [st]

conv_NFA_to_DFA (qs, sigma, delta, s, inF) =
    (qs', sigma, delta', s', inF') where
	qs' = powerset qs
	delta' [q] x = delta q x
	delta' (q:qs) x = delta q x ++ delta' qs x
	inF' q = any inF q
	s' = [s]

{- END PART TO FILL IN, PROBLEM 3 -}
