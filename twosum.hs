import Data.List (find)
import Data.Maybe (fromMaybe)

twoSum :: [Int] -> Int -> (Int,Int)
twoSum l n = head  pos
             where
               pos = dropWhile (\x -> snd x == fst x) [(p, p+1 + (findx (n-(l!!p)) (drop (p+1) l))) | p <- [0 ..length l]] 


                       
findx :: Int -> [Int] -> Int            
findx x l = fromMaybe (-1) (fst <$> (find ((==x) . snd) . zip [0..]) l)
