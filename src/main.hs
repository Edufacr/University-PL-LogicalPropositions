import Proposition(Proposition(..)) 
import Vars
import As_Vals

import EvalProp
import Fnd

a = Constant True

main :: IO ()
main = do 
    let propS = Conjuction (Variable "p") (Variable "q")
    let propM = Negation (Conjuction (Conjuction (Variable "p") (Variable "z")) (Variable "p"))
    let propL = Conditional (Conjuction (Negation (Variable "p")) (Variable "q")) (Conditional (Variable"q") (Variable "p"))
    let boolList = [True,False,True]
    print(fnd propL)
    print ( vars ( Conjuction (Variable "p") (Variable "q") ) )
    print ( vars (Negation (Variable "p")))
    print ( vars ( Conditional (Conjuction (Variable "p") (Variable "q")) (Disjunction (Variable "p") (Variable "q")) ) ) 