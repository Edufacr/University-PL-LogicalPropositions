module Taut(taut)
where 

import Proposition(Proposition(..))
import Vars(vars)
import GenBools(genBools)
import As_Vals(asVals)
import EvalProp(evalProp)
import Data.List


taut :: Proposition -> String
taut prop = 
    let 
        variables = vars prop
        n = toInteger(length variables)
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
                        (isEvaluationTrue && recorrer tailOfList)
            in 
                recorrer bools
    in
        if eval bools 
            then 
                "Es atutologia"
            else
                "No es tautologia"
