beeramid :: Double -> Double -> Int
beeramid m n =   length (takeWhile (<= (fst $ properFraction (m/n))) sumsq)
            where
            sumsq = [sum $ take n $ map (^2) [1..] | n <- [1..]]
