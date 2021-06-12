newfilter :: (a -> Bool) -> [a] -> [a]
newfilter f [] = []
newfilter f (x: xs) | f x == True = x : filter f xs
                 | f x == False = filter f xs

newrev :: [a] -> [a]
newrev = foldl (flip (:)) []

foldfilter :: (a -> Bool) -> [a] -> [a]
foldfilter f xs  = foldr (\x ls -> if f x == True then x:ls else ls )  [] xs

foldtakewhile ::(a -> Bool) -> [a] -> [a]
foldtakewhile f xs =  foldr (\x ls -> if f x == True then x:ls else [] )  [] xs
