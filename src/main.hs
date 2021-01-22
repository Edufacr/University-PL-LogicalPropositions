import Proposition(Proposition(..)) 
import Vars(vars)

main = do 
    print ( vars ( Conjuction (Variable "p") (Variable "q") ) )
    print (vars (Negation (Variable "p")))


  
