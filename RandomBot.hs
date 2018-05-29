module RandomBot
(
    randomLeaf
,   randomMove
,   extraRandomMove
) where

import AlphaBeta
import MiniMaxBot
import MetaTicTacToe
import System.Random


randomMove :: [Move] -> Integer -> Maybe Move
randomMove moves i = move `fmap` randomLeaf moves i

extraRandomMove :: RandomGen b => b -> [Move] -> Maybe Move
extraRandomMove gen moves
    | nextMoves == [] = Nothing
    | otherwise = Just (nextMoves !! (fst $ randomR (0, (length nextMoves) - 1) gen))
    where nextMoves = successor moves


randomLeaf :: [Move] -> Integer -> Maybe (Leaf Move)
randomLeaf moves i 
    | nextMoves == [] = Nothing
    | otherwise = Just (Leaf (nextMoves !! (fst $ randomR (0, (length nextMoves) - 1) (mkStdGen seed))) (utility $ botLeaf'))
    where seed = fst (move botLeaf') + (sum $ map fst moves)
          nextMoves = successor moves
          botLeaf' = case botLeaf moves i of Just m -> m
          