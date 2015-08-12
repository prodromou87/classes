{- CSE 105 FA 14: HW1 P3
 - Name: Andreas Prodromou
 - E-mail: aprodrom@eng.ucsd.edu
 - ID: A53049230
 - Collaborated with: None
 - 
 -}


module Main where

import DFA 
import Numeric 
import Data.Char

{-
 - note: the type for machine, below, assumes that you want states to
 - be numbered.  if for some reason you want some other type for the
 - states (e.g., String, for "state-one"), you'll need to change the
 - type from DFA Int to DFA String or similar.
-}

{- BEGIN PART TO FILL IN -}

m_qs :: [Int]
m_qs        = [0, 1, 2]

m_sigma :: [Char]
m_sigma     = [ '0', '1' ]

m_delta :: Int -> Char -> Int
m_delta 0 '0' = 0
m_delta 0 '1' = 1
m_delta 1 '0' = 2
m_delta 1 '1' = 0
m_delta 2 '0' = 1
m_delta 2 '1' = 2
m_delta _ _ = error "Unknown State"

m_s :: Int
m_s         = 0

m_inF :: Int -> Bool
m_inF 0     = True
m_inF _     = False

machine :: DFA Int
machine = (m_qs, m_sigma, m_delta, m_s, m_inF)
  
{- END PART TO FILL IN -}

{-
 - if your machine is correct, test n should return true
 - exactly when n is a multiple of 3:
 -}

bigTest :: Int -> [Bool]
bigTest n = map test (take n [ x | x <- [3,6..] ])

test :: Int -> Bool
test n = evalDFA machine (intToBinary n)

intToBinary x = showIntAtBase 2 intToDigit x ""
