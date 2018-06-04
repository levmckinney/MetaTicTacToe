module RandomBot
(
    randomLeaf
,   randomMove
,   genRandomMove
) where

import AlphaBeta
import MiniMaxBot
import MetaTicTacToe
import System.Random
import Data.Maybe

randomMove :: [Move] -> Maybe Move
randomMove moves
    | nextMoves == [] = Nothing
    | otherwise = Just $ nextMoves !! (fst $ randomR (0, (length nextMoves) - 1) (mkStdGen seed))
    where seed = sum $ map fst moves
          nextMoves = successor moves

genRandomMove :: RandomGen b => b -> [Move] -> Maybe Move
genRandomMove gen moves
    | nextMoves == [] = Nothing
    | otherwise = Just $ nextMoves !! (fst $ randomR (0, (length nextMoves) - 1) gen)
    where nextMoves = successor moves


randomLeaf :: [Move] -> Integer -> Maybe (Leaf Move)
randomLeaf moves i = maybe Nothing (\m -> Just (Leaf m 0)) (randomMove moves)