import Text.Printf (printf)
import Data.Function
-- This function should return a list [area, volume].
area::[Integer] -> [Integer] -> Integer -> Double
area w x y  =  sum $ zipWith (*) (zipWith ((/) `on`  fromInteger) w [l+1 | l <- x]) [fromInteger y^(k+1) | k <- x]
volume::[Integer] -> [Integer] -> Integer -> Double
volume w x y = sum $ zipWith (*) (zipWith ((/) `on`   fromInteger) w [(l+1)*(l+2) | l <- x]) [fromInteger y^(k+2) | k <- x]

solve :: Integer -> Integer -> [Integer] -> [Integer] -> [Double]
solve l r a b =  [(area  a b r) - (area a b l), (volume a b r) - (volume a b l)]

--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
