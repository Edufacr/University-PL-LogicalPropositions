import Proposition(Proposition(..)) 
import Vars(vars)
import Bonita(bonita)


a = Constant True

main :: IO ()
main = do 

    print ( bonita (Conditional (Conjuction (Variable "p") (Variable "q")) (Disjunction (Variable "p") (Variable "q"))) )
    
    print ( vars ( Conjuction (Variable "p") (Variable "q") ) )
    print ( vars (Negation (Variable "p")))
    print ( vars ( Conditional (Conjuction (Variable "p") (Variable "q")) (Disjunction (Variable "p") (Variable "q")) ) ) 