import Proposition(Proposition(..)) 
import Vars(vars)


a = Constant True

main :: IO ()
main = do 
    
    print ( vars ( Conjuction (Variable "p") (Variable "q") ) )
    print ( vars (Negation (Variable "p")))
    print ( vars ( Conditional (Conjuction (Variable "p") (Variable "q")) (Disjunction (Variable "p") (Variable "q")) ) ) 