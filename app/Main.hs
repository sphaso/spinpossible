module Main where

import System.Random
import Control.Monad
import Data.Char (isDigit)
import Data.Maybe (fromJust)

import Lib
import Types

randint :: IO Int
randint = getStdRandom $ randomR (1, 9)

randomMoves :: IO [Int]
randomMoves = replicateM 16 randint

couple :: [a] -> [(a, a)]
couple [] = []
couple (a:b:c:d:xs) = (zip [a, b, c, d] xs) ++ couple (drop 4 xs)

maybeRead :: String -> Maybe Int
maybeRead s | all isDigit s = Just $ read s
            | otherwise     = Nothing

loop :: Grid -> IO ()
loop  g = do
          putStrLn "Give two numbers corresponding to the upper left and bottom right corners of the submatrix"
          a <- maybeRead <$> getLine
          b <- maybeRead <$> getLine
          case rotate' (numberToCoord =<< a) (numberToCoord =<< b) g of
              Left s     -> (putStrLn s >> loop g)
              Right newg -> checkCompleteness newg

checkCompleteness :: Grid -> IO ()
checkCompleteness g | completed g = putStrLn "Congratulations! You win!"
                    | otherwise   = (putStrLn (show g) >> loop g)

rotate' :: Maybe Coord -> Maybe Coord -> Grid -> Either String Grid
rotate' (Just a) (Just b) g = Right $ rotate a b g
rotate' _        _        _ = Left "Input needs to be between 1 and 9"

main :: IO ()
main = do
        moves <- couple <$> map (fromJust . numberToCoord) <$> randomMoves
        let newg = foldr (\(a, b) g -> rotate a b g) pristineGrid moves
        putStrLn $ show newg
        loop newg
