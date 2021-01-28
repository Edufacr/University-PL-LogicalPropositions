module Bonita(bonita, bonita')
where

    import Proposition(Proposition(..))

    bonita :: Proposition -> String
    bonita prop = show prop ++ "\n" ++bonita' prop

    bonita' :: Proposition -> String
    bonita'(Variable prop1) = prop1 
    bonita'(Negation prop1) = 
        case prop1 of  
            (Constant propInside) -> (if propInside then "False" else "True")
            (Variable propInside) -> "~" ++ propInside
            _                     -> "~(" ++ bonita' prop1 ++ ")"
    bonita'(Constant prop1) = if prop1 then "True" else "False"
    bonita'(Conjuction prop1 prop2) = 
        let mitad1 = 
                if presedencia prop2 > presedencia (Conjuction prop1 prop2) 
                then 
                "("++ bonita' prop1 ++ ")"
                else bonita' prop1
            
            mitad2 = 
                if presedencia prop2 > presedencia (Conjuction prop1 prop2) 
                then 
                "("++ bonita' prop2 ++ ")"
                else bonita' prop2
        in 
            mitad1 ++ "^" ++ mitad2

    bonita'(Disjunction prop1 prop2) = 
        let mitad1 = 
                if presedencia prop2 > presedencia (Disjunction prop1 prop2) 
                then 
                "("++ bonita' prop1 ++ ")"
                else bonita' prop1
            
            mitad2 = 
                if presedencia prop2 > presedencia (Disjunction prop1 prop2) 
                then 
                "("++ bonita' prop2 ++ ")"
                else bonita' prop2
        in 
            mitad1 ++ "v" ++ mitad2

    bonita'(Conditional prop1 prop2) = 
        let mitad1 = 
                if presedencia prop2 > presedencia (Conditional prop1 prop2) 
                then 
                "("++ bonita' prop1 ++ ")"
                else bonita' prop1
            
            mitad2 = 
                if presedencia prop2 > presedencia (Conditional prop1 prop2) 
                then 
                "("++ bonita' prop2 ++ ")"
                else bonita' prop2
        in 
            mitad1 ++ "=>" ++ mitad2

    bonita'(Biconditional prop1 prop2) = 
        let mitad1 = 
                if presedencia prop2 > presedencia (Biconditional prop1 prop2) 
                then 
                "("++ bonita' prop1 ++ ")"
                else bonita' prop1
            
            mitad2 = 
                if presedencia prop2 > presedencia (Biconditional prop1 prop2) 
                then 
                "("++ bonita' prop2 ++ ")"
                else bonita' prop2
        in 
            mitad1 ++ "<=>" ++ mitad2

            
    presedencia :: Proposition -> Int
    presedencia (Conjuction prop1 prop2)    = 4
    presedencia (Disjunction prop1 prop2)   = 3
    presedencia (Conditional prop1 prop2)   = 2
    presedencia (Biconditional prop1 prop2) = 1
    presedencia _                           = 0
