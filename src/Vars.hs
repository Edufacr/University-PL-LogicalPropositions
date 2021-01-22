module Vars(vars)
where
    
import Proposition(Proposition(..))

vars :: Proposition -> [String]

vars (Variable var) = [var]
vars (Negation prop) = vars prop
-- ? Debe haber una forma de poner estas en una sola 
vars (Conjuction prop1 prop2) = vars prop1 ++ vars prop2
vars (Disjunction prop1 prop2) = vars prop1 ++ vars prop2
vars (Conditional prop1 prop2) = vars prop1 ++ vars prop2
vars (Biconditional prop1 prop2) = vars prop1 ++ vars prop2

rmDupli :: [*] -> [*]
rmDupli [] = []
--rmDupli (x:xs) =  
