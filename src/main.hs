import Proposition(Proposition(..)) 
import Vars
import As_Vals
import EvalProp
import Fnd
import Bonita
import GenBools
import Taut

import qualified Control.Exception as Exc

main :: IO ()
main = do 
    --Tautologia
    let prop1 = Conditional (Conjuction (Variable "p") (Variable "q")) (Disjunction (Variable "p") (Variable "q"))
    -- 4 Variables y una Constante (No es tautologia)
    let prop2 = Conditional (Biconditional (Negation (Conjuction (Conjuction (Variable "p") (Variable "r")) (Variable "q"))) (Constant False)) (Variable "s")
    -- Solo constantes
    let prop3 = Conditional (Conjuction (Negation (Constant True)) (Constant False)) (Conditional (Constant True) (Constant True))
    
    let boolList = [True,True,True,False]
    let boolList2 = [False,True,True,False]

    printFooName "Vars"
    putStrLn "\nCaso donde se repiten variables"
    printVarsTest 1 prop1
    putStrLn "\nCaso con 4 variables"
    printVarsTest 2 prop2
    putStrLn "\nCaso donde donde no hay variables"
    printVarsTest 3 prop3

    printFooName "Gen_Bools"
    putStrLn "\nCaso donde el numero de variables es 0"
    printGenBoolsTest 1 0
    putStrLn "\nCaso donde el numero de variables es -1"
    printGenBoolsTest 2 (-1)
    putStrLn "\nCaso donde el numero de variables es 1"
    printGenBoolsTest 3 1
    putStrLn "\nCaso donde el numero de variables es 2"
    printGenBoolsTest 3 2

    printFooName "As_Vals"
    putStrLn "\nCaso donde el tamaño de las listas es igual"
    printAsValsTest 1 ["p","q","r"] [True,False,False]
    putStrLn "\nCaso donde la lista de bools es mas grande que la de variables"
    printAsValsTest 2 ["p"] [True,False,False]
    putStrLn "\nCaso donde la lista de variables es mas grande que la de bools"
    printAsValsTest 3 ["p","q","r"] [True]
    putStrLn "\nCaso donde las listas son vacias"
    printAsValsTest 4 [] []

    printFooName "Eval_Prop"
    putStrLn "\nCaso donde no hay variables"
    printEvalPropTest 1 prop3 []
    putStrLn "\nCaso donde el resultado es true"
    printEvalPropTest 2 prop2 (asVals (vars prop2) boolList)
    putStrLn "\nCaso donde el resultado es false"
    printEvalPropTest 3 prop2 (asVals (vars prop2) boolList2)
    putStrLn "\nCaso donde las listas de valores es vacia"
    printEvalPropTest 4 prop2 []

    printFooName "Taut"
    putStrLn "\nCaso donde es tautologia"
    printTautTest 1 prop1
    putStrLn "\nCaso donde no es tautologia"
    printTautTest 2 prop2
    putStrLn "\nCaso donde no hay variables"
    printTautTest 3 prop3

    printFooName "Fnd"

    putStrLn "\nCaso con variables repetidas"
    printFndTest 1 prop1
    putStrLn "\nCaso con 4 varibales y todas las proposiciones"
    printFndTest 2 prop2
    putStrLn "\nCaso donde no hay variables"
    printFndTest 3 prop3
    putStrLn "\nCaso donde la proposicion es una contradiccion"
    printFndTest 4 (Conjuction (Negation (Variable "p")) (Variable "p"))
    putStrLn "\nCaso donde ya se encuentra en forma normal"
    printFndTest 5 (Disjunction (Conjuction (Negation (Variable "p")) (Variable "q")) (Conjuction (Negation (Variable "q")) (Variable "p")))

    printFooName "Bonita"
    putStrLn "\nCaso donde se encuentran todas las proposiciones"
    putStrLn (bonita prop2)
    putStrLn "\nCaso donde se encuentra una constante negada"
    putStrLn (bonita (Negation (Constant False)))
    putStrLn "\nCaso con triple negacion"
    putStrLn (bonita (Conjuction(Negation (Negation (Negation (Variable "p")))) (Constant True) ) )


printFooName :: String -> IO() 
printFooName name = 
    do 
        putStrLn ""
        putStrLn "* * * * * *"
        putStrLn ("Function: " ++ name)
        putStrLn "* * * * * *"
        putStrLn ""
printTestIntro:: Int -> IO() 
printTestIntro num = 
    do 
        putStrLn ""
        putStrLn ("Test: " ++ show num)

printVarsTest :: Int -> Proposition -> IO()
printVarsTest num prop = 
    do
        printTestIntro num
        putStrLn ("Proposition: " ++ bonita' prop)
        putStrLn "List of Variables:"
        print(vars prop)

printGenBoolsTest :: Int -> Int -> IO()
printGenBoolsTest testNum n = 
    do
        printTestIntro testNum
        putStrLn ("N = " ++ show n)
        putStrLn "Output:"
        print(genBools n)

printAsValsTest :: Int -> [String] -> [Bool] -> IO()
printAsValsTest num varsList bools = 
    do 
        printTestIntro num 
        putStrLn "List of Variables:"
        print varsList
        putStrLn "List of Bools:"
        print bools
        putStrLn "Output:"
        Exc.catch (print(asVals varsList bools)) handler

printEvalPropTest :: Int -> Proposition -> [(String,Bool)] -> IO()
printEvalPropTest testNum prop vals = 
    do
        printTestIntro testNum
        putStrLn ("Proposition: " ++ bonita' prop)
        putStrLn "Values:"
        print vals
        putStrLn "Result: "
        Exc.catch (print(evalProp prop vals)) handler

printTautTest :: Int -> Proposition -> IO()
printTautTest testNum prop =
    do
        printTestIntro testNum
        putStrLn ("Proposition: " ++ bonita' prop)
        putStrLn "Output: "
        putStrLn (taut prop)

printFndTest :: Int -> Proposition -> IO()
printFndTest testNum prop =
    do
        printTestIntro testNum
        putStrLn ("Proposition: " ++ bonita' prop)
        putStrLn "Output: "
        putStrLn (bonita' (fnd prop))
        

handler :: Exc.ErrorCall -> IO ()
handler e = putStrLn ("Error: " ++ show e)
