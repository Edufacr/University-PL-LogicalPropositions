module EvalProp(evalProp) where

import Proposition(Proposition(..))

evalProp:: Proposition -> [(String,Bool)] -> Bool 

evalProp (Constant val) _ = val
evalProp (Variable var) asVals = getValue var asVals
evalProp (Negation prop) asVals = not (evalProp prop asVals)
evalProp (Conjuction prop1 prop2) asVals = evalProp prop1 asVals && evalProp prop2 asVals
evalProp (Disjunction prop1 prop2) asVals = evalProp prop1 asVals || evalProp prop2 asVals
evalProp (Conditional prop1 prop2) asVals = evalProp prop1 asVals --> evalProp prop2 asVals
evalProp (Biconditional prop1 prop2) asVals = evalProp prop1 asVals <--> evalProp prop2 asVals

-- Function to represent Biconditional logic operator
(<-->) :: Bool -> Bool -> Bool
(<-->) val1 val2 = val1 --> val2 && val2 --> val1

-- Function to represent Conditional logic operator
(-->) :: Bool -> Bool -> Bool
(-->) val1 val2 = not val1 || val2

getValue:: String -> [(String,Bool)] -> Bool
getValue _ [] = error "Error: Variable value not found"
getValue var (x:xs)
    | var == fst x = snd x
    | otherwise = getValue var xs

