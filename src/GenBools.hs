module GenBools(genBools)
where 


cons :: e -> [e] -> [e]
cons x xs = x : xs

genBools :: Integer -> [[Bool]]
genBools 0 = [[]]
genBools n = 
    let anterior = genBools (n-1)
    in map(cons True) anterior ++ map(cons False) anterior 