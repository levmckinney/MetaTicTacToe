module RandomBot
(
    randomLeaf
,   randomMove
) where

import AlphaBeta
import MiniMaxBot
import MetaTicTacToe
import System.Random

randomMove :: [Move] -> Integer -> Maybe Move
randomMove moves i = move `fmap` randomLeaf moves i

randomLeaf :: [Move] -> Integer -> Maybe (Leaf Move)
randomLeaf moves i 
    | nextMoves == [] = Nothing
    | otherwise = Just (Leaf (nextMoves !! (fst $ randomR (0, ((length nextMoves) - 1)) (mkStdGen seed))) (utility $ botLeaf'))
    where seed = fst (move botLeaf') + (sum $ map fst moves)
          nextMoves = successor moves
          botLeaf' = Leaf (0,0) 0 --case botLeaf moves i of Just m -> m
          