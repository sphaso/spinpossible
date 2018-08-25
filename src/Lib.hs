module Lib
    ( numberToCoord
    , coordToNumber
    , rotate
    , completed
    , pristineGrid
    ) where

import Data.List (sort)

import Types

numberToCoord :: Int -> Coord
numberToCoord 1 = (0, 0)
numberToCoord 2 = (0, 1)
numberToCoord 3 = (0, 2)
numberToCoord 4 = (1, 0)
numberToCoord 5 = (1, 1)
numberToCoord 6 = (1, 2)
numberToCoord 7 = (2, 0)
numberToCoord 8 = (2, 1)
numberToCoord 9 = (2, 2)

coordToNumber :: Coord -> Int
coordToNumber (a, b) = 3 * a + b + 1

completed :: Grid -> Bool
completed = all (\(Tile c n s) -> s && n == (coordToNumber c))

rotate :: Coord -> Coord -> Grid -> Grid
rotate _ _ [] = []
rotate (u, d) (u2, d2) xs = sort $ without ++ (revv within)
    where within  = filter (\(Tile (tu, td) _ _) -> tu >= u && tu <= u2 && td >= d && td <= d2) xs
          without = filter (\a -> not $ elem a within) xs

revv :: Grid -> Grid
revv xs = map (\((Tile p n s), (Tile p2 n2 s2)) -> Tile p n2 (not s2)) $ zip xs (reverse xs)

pristineGrid :: Grid
pristineGrid = [Tile (numberToCoord n) n True | n<-[1..9]]
