module As_Vals(asVals)
where
    
import Proposition(Proposition(..))

asVals:: [String] -> [Bool] -> [(String, Bool)]

asVals = zipVarVals

-- Checks if bool list lenght >= var list lenght
zipVarVals:: [String] -> [Bool] -> [(String,Bool)]

zipVarVals (x:xs) (y:ys) = (x,y) : zipVarVals xs ys
zipVarVals [] _ = []
zipVarVals _ _ = error "Not enough values given"