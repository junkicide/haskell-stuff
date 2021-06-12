findOdd :: [Int] -> Int
findOdd (x:[]) = x
findOdd (x:xs) = if odd $ length filt then findOdd filt else x 
                 where
                   filt = filter (x/=) xs
