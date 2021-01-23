module Vars(vars)
where

import Proposition(Proposition(..))


vars :: Proposition -> [String]

vars p = nub(vars' p)
    where 
        vars' (Constant val)                = []
        vars' (Variable var)                = [var]
        vars' (Negation prop)               = vars' prop
        vars' (Conjuction prop1 prop2)      = vars' prop1 ++ vars' prop2
        vars' (Disjunction prop1 prop2)     = vars' prop1 ++ vars' prop2
        vars' (Conditional prop1 prop2)     = vars' prop1 ++ vars' prop2
        vars' (Biconditional prop1 prop2)   = vars' prop1 ++ vars' prop2

----------------------------------------------
-- Based on Nub from Data.List
-- Author: The University of Glasgow 2001
----------------------------------------------
nub :: (Eq e) => [e] -> [e]

nub list = nub' list []
    where 
        nub' [] _                               = []
        nub' (first:left_of_list) foundElements
            | foundElements `contains` first    = nub' left_of_list foundElements
            | otherwise                         = first : nub' left_of_list (first:foundElements)

contains:: (Eq e) => [e] -> e -> Bool

contains [] _ = False 
contains (x:xs) e
    | x == e = True
    | otherwise = contains xs e




