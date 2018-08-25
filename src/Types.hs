module Types where

data Tile = Tile {position :: Coord, number :: Int, straight :: Bool} deriving (Eq)

instance Show Tile where
    show (Tile _ n s) = (star s) ++ (show n)
          where star True = "*"
                star False = ""

instance Ord Tile where
    compare (Tile (a, b) _ _) (Tile (c, d) _ _) | a == c && b == d = EQ
                                                | a > c = GT
                                                | a < c = LT
                                                | b > d = GT
                                                | otherwise = LT

type Grid = [Tile]

type Coord = (Int, Int)
