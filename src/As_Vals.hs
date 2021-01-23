module As_Vals(asVals)
where
    
import Proposition(Proposition(..))

asVals:: [String] -> [Bool] -> [(String, Bool)]

asVals pList bList 
    | length pList == length bList = zip1 pList bList
    | otherwise = []

zip1:: [a] -> [b] -> [(a,b)]

zip1 [] [] = []
zip1 (x:xs) (y:ys) = (x,y) : zip1 xs ys