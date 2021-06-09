import Data.Char
import Data.List
import System.IO

size :: Int
size = 3

type Grid = [[Player]]

data Player = 0 | B | X deriving (Eq, Ord, Show)

next :: Player -> Player 
next 0 = X
next B = B
next X = 0


empty:: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then 0 else X
  where
    os = length (filter (==0) ps)
    xs = length (filter (==X) ps)
    ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where
   line = all (==p)
   rows = g
   cols = transpose g
   dias = [diag g, diag (map reverse g)]

    
