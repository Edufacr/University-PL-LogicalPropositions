module GenBools(genBools)
where 


cons :: e -> [e] -> [e]
cons x xs = x : xs

genBools :: Int -> [[Bool]]
genBools 0 = [[]]
genBools n 
    | n > 0 = map(cons True) anterior ++ map(cons False) anterior
    | otherwise = [[]]
    where anterior = genBools (n-1)