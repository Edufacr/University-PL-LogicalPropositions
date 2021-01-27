import Proposition(Proposition(..)) 
import Vars
import As_Vals
import EvalProp
import Fnd
import Bonita
import GenBools
import Taut

import qualified Control.Exception as Exc

a = Constant True




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
    printVarsTest 1 prop1
    printVarsTest 2 prop2
    printVarsTest 3 prop3

    printFooName "Gen_Bools"
    printGenBoolsTest 1 0
    printGenBoolsTest 2 1
    printGenBoolsTest 3 3

    printFooName "As_Vals"
    printAsValsTest 1 ["p","q","r"] [True,False,False]
    printAsValsTest 2 ["p"] [True,False,False]
    printAsValsTest 3 ["p","q","r"] [True]
    printAsValsTest 4 [] []

    printFooName "Eval_Prop"
    printEvalPropTest 1 prop3 []
    printEvalPropTest 2 prop2 (asVals (vars prop2) boolList)
    printEvalPropTest 3 prop2 (asVals (vars prop2) boolList2)
    printEvalPropTest 4 prop2 []

    printFooName "Taut"
    printTautTest 1 prop1
    printTautTest 2 prop2
    printTautTest 3 prop3

    printFooName "Fnd"

    printFndTest 1 prop1
    printFndTest 2 prop2
    printFndTest 3 prop3
    printFndTest 4 (Conjuction (Negation (Variable "p")) (Variable "p"))
    printFndTest 5 (Disjunction (Conjuction (Negation (Variable "p")) (Variable "q")) (Conjuction (Negation (Variable "q")) (Variable "p")))

    printFooName "Bonita"
    putStrLn (bonita (Negation (Constant False)))
    putStrLn ( bonita (Conjuction(Negation (Negation (Negation (Variable "p")))) (Constant True) ) )







printFooName :: String -> IO() 
printFooName name = 
    do 
        putStrLn ""
        putStrLn "* * * * * *"
        putStrLn ("Function: " ++ name)
        putStrLn "* * * * * *"
printTestIntro:: Int -> IO() 
printTestIntro num = 
    do 
        putStrLn ""
        putStrLn ("Test: " ++ show num)

printVarsTest :: Int -> Proposition -> IO()
printVarsTest num prop = 
    do
        printTestIntro num
        putStrLn ("Proposition: " ++ bonita prop)
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
        putStrLn ("Proposition: " ++ bonita prop)
        putStrLn "Values:"
        print vals
        putStrLn "Result: "
        Exc.catch (print(evalProp prop vals)) handler

printTautTest :: Int -> Proposition -> IO()
printTautTest testNum prop =
    do
        printTestIntro testNum
        putStrLn ("Proposition: " ++ bonita prop)
        putStrLn "Output: "
        putStrLn (taut prop)

printFndTest :: Int -> Proposition -> IO()
printFndTest testNum prop =
    do
        printTestIntro testNum
        putStrLn ("Proposition: " ++ bonita prop)
        putStrLn "Output: "
        putStrLn (bonita (fnd prop))
        

handler :: Exc.ErrorCall -> IO ()
handler e = putStrLn ("Error: " ++ show e)
