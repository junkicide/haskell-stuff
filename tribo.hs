

tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci (a, b, c) n | n>3 = tr ++ [ (sum $ drop  (l-3) tr)]
                       | n<=3 = take n  [a, b, c]
                        where tr = tribonacci (a, b, c) (n-1)
                              l=length tr



 

