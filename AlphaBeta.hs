module AlphaBeta
(
   miniMax
,  miniMaxLeaf
,  integerNegInfinity
,  integerInfinity
,  Leaf(..)
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
                  searchStepAlpha alphaLeaf move
                    | utility alphaLeaf < ib = if utility childAlphaLeaf > utility alphaLeaf
                                               then childAlphaLeaf
                                               else alphaLeaf
                    |otherwise = alphaLeaf
                    where childAlphaLeaf = childLeaf move (utility alphaLeaf, ib)
                  searchStepBeta betaLeaf move
                    | ia > utility betaLeaf = if utility childBetaLeaf < utility betaLeaf
                                              then childBetaLeaf
                                              else betaLeaf
                    | otherwise = betaLeaf
                    where childBetaLeaf = childLeaf move(ia, utility betaLeaf)

integerInfinity :: Integer
integerInfinity = 100000
integerNegInfinity :: Integer
integerNegInfinity = -integerInfinity
                    