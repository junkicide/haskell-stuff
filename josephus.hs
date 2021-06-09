josephus :: [a] -> Int -> [a]
josephus [] k = []
josephus xs k | length (fst s) /= 1 = last (fst s): josephus (snd s ++ (init $ fst s)) k
              | otherwise = last (fst s):josephus (snd s) k
              where
                s = splitAt (mod (k-1) (length xs) + 1) xs
