module MiniMax
(
   miniMax
,  miniMaxLeaf
,  integerNegInfinity
,  integerInfinity
) where

import MetaTicTacToeBot

miniMax :: [m] -> Integer -> ([m] -> [m]) -> ([m] -> Integer) -> m
miniMax startMoves maxDepth sucFunc utilityFunc = move (miniMaxLeaf startMoves maxDepth sucFunc utilityFunc)

miniMaxLeaf :: [m] -> Integer -> ([m] -> [m]) -> ([m] -> Integer) -> Leaf m
miniMaxLeaf startMoves maxDepth sucFunc utilityFunc = findMiniMaxLeaf maxDepth True startMoves
    where findMiniMaxLeaf depth isMax moves 
            | length (sucFunc moves) == 0 || depth == 0 = Leaf (moves !! (length startMoves)) (utilityFunc moves)
            | isMax = foldl (\bestChild childMoves -> orderUtility bestChild (<) (childLeaf childMoves)) (childLeaf (nextMoves !! 0)) nextMoves
            | not isMax = foldl (\worstChild childMoves -> orderUtility worstChild (>) (childLeaf childMoves)) (childLeaf (nextMoves !! 0)) nextMoves
            where nextMoves = map (\nextMove -> moves ++ [nextMove]) (sucFunc moves)
                  childLeaf = findMiniMaxLeaf (depth - 1) (not isMax)
                  orderUtility leaf1 gtlt leaf2 = if utility leaf1 `gtlt` utility leaf2 then leaf2 else leaf1

integerInfinity :: Integer
integerInfinity = 10000000
integerNegInfinity :: Integer
integerNegInfinity = -integerInfinity
