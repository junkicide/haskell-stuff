
isPrime :: Integer -> Bool

isPrime x | x == 2 = True
          | x<2    = False
          | x>2    = and [x `mod` d > 0 | d <- sieve [2..n]]
                       where
                         n = ceiling $ sqrt $ fromInteger x
         

sieve (p : xs) = p : sieve [n | n <- xs, n `mod` p > 0]
sieve [] = []
