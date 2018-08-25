module Main where

import Control.Monad.State
import System.Random

import Lib
import Types

stRotate :: Coord -> Coord -> State Grid ()
stRotate c d = do
                g <- get
                put $ rotate c d g

randint :: State StdGen Int
randint = state $ randomR (1, 9)

randomMoves = replicateM 16 randint

pseudoShuffle :: [a] -> [(a, a)]
pseudoShuffle [] = []
pseudoShuffle (a:b:c:d:xs) = (zip [a, b, c, d] xs) ++ pseudoShuffle (drop 4 xs)

loop :: Grid -> IO ()
loop  g = do
          putStrLn "Give two numbers corresponding to the upper left and bottom right corners of the submatrix"
          a <- getLine
          b <- getLine
          let newg = execState (stRotate (numberToCoord $ read a) (numberToCoord $ read b)) g
          case completed newg of
              True  -> putStrLn "Congratulations! You win!"
              False -> do
                         putStrLn $ show newg
                         loop newg

main :: IO ()
main = do
        let moves = pseudoShuffle $ map numberToCoord $ evalState randomMoves (mkStdGen 7) 
        let newg = execState (mapM_ (\(a, b) -> stRotate a b) moves) pristineGrid
        putStrLn $ show newg
        loop newg
