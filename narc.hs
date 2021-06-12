narcissistic :: Integral n => n -> Bool
narcissistic n = ( sum $ map (^l) d) == n
                  where
                  d = dig n
                  l = length d

dig:: Integral a => a ->  [a]
dig 0 = []
dig n = dig (n `div` 10) ++ [(n`mod` 10)]


