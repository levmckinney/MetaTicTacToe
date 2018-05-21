module MiniMax
(
  miniMax
) where

data Leaf m = Leaf {move :: m, utility :: Integer}

miniMax :: [m] -> Integer -> ([m] -> [[m]]) -> ([m] -> Integer) -> m
miniMax startMoves maxDepth sucFunc utilityFunc = move (findMiniMaxLeaf maxDepth True startMoves)
    where findMiniMaxLeaf depth isMax moves 
            | length (sucFunc moves) == 0 || depth == 0 = Leaf (head moves) (utilityFunc moves)
            | isMax = foldl (\bestChild childMoves -> orderUtility bestChild (>) (childLeaf childMoves)) (childLeaf (nextMoves !! 0)) nextMoves
            | not isMax = foldl (\worstChild childMoves -> orderUtility worstChild (<) (childLeaf childMoves)) (childLeaf (nextMoves !! 0)) nextMoves
            where nextMoves = map ((++) moves) (sucFunc moves)
                  childLeaf = findMiniMaxLeaf (depth - 1) (not isMax)
                  orderUtility leaf1 gtlt leaf2 = if utility leaf1 `gtlt` utility leaf2 then leaf2 else leaf1


integerInfinity = 10000
integerNegInfinity = -integerInfinity
