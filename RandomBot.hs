module RandomBot
(
    randomLeaf
,   randomMove
) where

import AlphaBeta
import MiniMaxBot
import MetaTicTacToe
import System.Random
import Data.Maybe

randomMove :: StdGen -> [Move] -> Maybe Move
randomMove gen moves
    | nextMoves == [] = Nothing
    | otherwise = Just $ nextMoves !! (fst $ randomR (0, (length nextMoves) - 1) gen)
    where nextMoves = successor moves


randomLeaf :: StdGen -> [Move] -> Integer -> Maybe (Leaf Move)
randomLeaf gen moves i = maybe Nothing (\m -> Just (Leaf m 0)) (randomMove gen moves)