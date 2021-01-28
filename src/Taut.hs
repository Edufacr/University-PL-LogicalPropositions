module Taut(taut)
where 

import Proposition(Proposition(..))
import Vars(vars)
import GenBools(genBools)
import As_Vals(asVals)
import EvalProp(evalProp)
import Bonita(bonita)
import Data.List
import Debug.Trace (traceShow)


taut :: Proposition -> String
taut prop = 
    let 
        variables = vars prop
        n = length variables
        bools = genBools n

        eval :: [[Bool]] -> Bool
        eval [] = True
        eval booleans = 
            let 
                recorrer :: [[Bool]] -> Bool 
                recorrer [] = True
                recorrer values = 
                    let 
                        headOfList = head values
                        tailOfList = tail values
                        asignacion = asVals variables headOfList
                        isEvaluationTrue = evalProp prop asignacion
                    in 
                        if isEvaluationTrue
                            then recorrer tailOfList
                        else
                            traceShow ( bonita prop ++ (" No es tautologia para la asignacion " ++ intercalate ", " variables ++ " con " ++ intercalate ", " (map boolToString headOfList)  ))
                            False
                        --(isEvaluationTrue && recorrer tailOfList)
            in 
                recorrer bools
    in
        if eval bools 
            then 
                bonita prop ++ " Es tautologia"
            else
                "" 

boolToString :: Bool -> String
boolToString val = if val then "True" else "False"