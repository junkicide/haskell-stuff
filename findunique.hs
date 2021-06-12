getUnique :: [Int] -> Int
getUnique l =  let newl = dropWhile (head l ==) (tail l) in if head newl == head(tail newl) || tail l = [] then head l else head newl
