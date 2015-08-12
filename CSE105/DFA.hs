{- |
Module          : DFA
Description     : Definition of Deterministic Finite Automata
Author          : Daniele Micciancio
Stability       : experimental
-}
module DFA where

-- |Data type representing a Deterministic Finite Automaton, parametrized by a type of states.
type DFA st = ([st], [Char], st->Char->st, st, st->Bool)

-- | Function f_M: Sigma* -> {True,False} defined by DFA M
-- Uses auxiliary function delta*: Q x Sigma* -> Q
evalDFA :: DFA st -> String -> Bool
evalDFA (qs, sigma, delta, s, inF) w = 
  inF (deltaStar s w) 
  where deltaStar q [] = q
        deltaStar q (a:w) = deltaStar (delta q a) w

{-| Alternative definition of the function computed by a DFA using configurations and a transition relation.
    It can be checked that for all DFA m and input string w, (evalDFA m w) == (execDFA m w)  
-} 

-- |DFA configurations, parametrized by the state type
type ConfigDFA st = (st,String)

-- |Function mapping input strings to the corresponding initial configuration of a DFA
initConfigDFA :: DFA st -> String -> ConfigDFA st
initConfigDFA (qs, sigma, delta, s, inF) w = (s, w)

-- |Function mapping a configuration C1 to
--  (Nothing)  if C1 is a halting configuration
--  (Just C2)  if C2 is the next configuration in a valid computation
nextConfigDFA :: DFA st -> ConfigDFA st -> Maybe (ConfigDFA st)
nextConfigDFA (qs, sigma, delta, s, inF) (q,[])   = Nothing
nextConfigDFA (qs, sigma, delta, s, inF) (q,a:w)  = Just (delta q a, w)

-- |Function testing if a haling configuration is accepting or not
acceptConfigDFA :: DFA st -> ConfigDFA st -> Bool
acceptConfigDFA (qs, sigma, delta, s, inF) (q,[]) = inF q

-- |Run a DFA on an input string, until it reaches a final configuration
runDFA :: DFA st -> String -> ConfigDFA st
runDFA dfa w = run (initConfigDFA dfa w) where
  run conf = 
    case (nextConfigDFA dfa conf) of 
      Nothing      -> conf         -- a final configuration has been reached
      Just newConf -> run newConf  -- otherwise, keep running

-- |Similar to runDFA, but just output True or False, rather than the final configuration
execDFA :: DFA st -> String -> Bool
execDFA dfa w = acceptConfigDFA dfa (runDFA dfa w)

