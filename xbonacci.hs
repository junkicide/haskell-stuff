

xbonacci :: Num a => [a] -> Int -> [a]
xbonacci _ 0 = []
xbonacci [] _ = []
xbonacci (x:xs) n = x: xbonacci (xs++[s]) (n-1)
                    where
                      s = sum (x:xs)



 

