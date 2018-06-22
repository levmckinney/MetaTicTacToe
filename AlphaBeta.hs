module AlphaBeta
(
   miniMax
,  miniMaxLeaf
,  integerNegInfinity
,  integerInfinity
) where

import MetaTicTacToeBot

type NodeState = (Integer,Integer)

--this is a gerneral implementaion of the minimax algorithem
miniMax :: [m] -> Integer -> ([m] -> [m]) -> ([m] -> Integer) -> m
miniMax startMoves maxDepth sucFunc utilityFunc = move (miniMaxLeaf startMoves maxDepth sucFunc utilityFunc)

--This function return the Leaf that is return by the minimax. (the utility can be used to see how well the bot thinks its doing).
miniMaxLeaf :: [m] -> Integer -> ([m] -> [m]) -> ([m] -> Integer) -> Leaf m
miniMaxLeaf startMoves maxDepth sucFunc utilityFunc = alphaBetaFindLeaf maxDepth True startMoves (integerNegInfinity, integerInfinity)
    where alphaBetaFindLeaf depth isMax moves nodeState@(ia, ib) -- the recursive implenentaion of Alpha Beta Pruning
            | length possibleMoves == 0 || depth == 0 = Leaf (moves !! (length startMoves)) (utilityFunc moves)
            | isMax     = foldl searchStepAlpha firstChildLeaf possibleMoves
            | not isMax = foldl searchStepBeta firstChildLeaf possibleMoves
            where firstChildLeaf = childLeaf (possibleMoves !! 0) nodeState
                  possibleMoves = sucFunc moves
                  childLeaf move = alphaBetaFindLeaf (depth - 1) (not isMax) (moves ++ [move])
                  searchStepAlpha valueLeaf move
                    | ib > utility valueLeaf  = if utility childValueLeaf > utility valueLeaf
                                                then childValueLeaf
                                                else valueLeaf
                    |otherwise = valueLeaf
                      where childValueLeaf = childLeaf move (utility valueLeaf, ib)
                  searchStepBeta valueLeaf move
                    | ia < utility valueLeaf = if utility childValueLeaf < utility valueLeaf
                                               then childValueLeaf
                                               else valueLeaf
                    | otherwise = valueLeaf
                      where childValueLeaf = childLeaf move (ia, utility valueLeaf)

integerInfinity :: Integer
integerInfinity = 10000000
integerNegInfinity :: Integer
integerNegInfinity = -integerInfinity
                    