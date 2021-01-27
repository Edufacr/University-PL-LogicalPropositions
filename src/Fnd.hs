module Fnd
where
import Proposition(Proposition(..))
import GenBools ( genBools )
import EvalProp ( evalProp )
import As_Vals ( asVals )
import Vars ( vars )

fnd :: Proposition -> Proposition

fnd prop        = fnd' (filter (evalProp prop) varVals)
    where 
        varList = vars prop
        varVals = map (asVals varList) (genBools (length varList))

fnd' :: [[(String,Bool)]] -> Proposition 

fnd' []     = Constant False
fnd' [[]]   = Constant False
fnd' (x:[]) = primeTerm x
fnd' (x:xs) = Disjunction (primeTerm x) (fnd' xs)

primeTerm :: [(String, Bool)] -> Proposition 

primeTerm ((var, val):[])
    | val       = Variable var
    | otherwise = Negation (Variable var)

primeTerm ((var, val):xs)
    | val       = Conjuction (Variable var) (primeTerm xs)
    | otherwise = Conjuction (Negation (Variable var)) (primeTerm xs)
