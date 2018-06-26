module AlphaBeta
(
   miniMax
,  miniMaxLeaf
,  integerNegInfinity
,  integerInfinity
) where

import MetaTicTacToeBot

type NodeState = (Integer,Integer)

--Version that simply returns a move.
miniMax :: [m] -> Integer -> ([m] -> [m]) -> ([m] -> Integer) -> m
miniMax startMoves maxDepth sucFunc utilityFunc = move (miniMaxLeaf startMoves maxDepth sucFunc utilityFunc)

--This is the main impementation of the minimax algorithem with alpha beta pruning. link to explenation: https://www.hackerearth.com/blog/artificial-intelligence/minimax-algorithm-alpha-beta-pruning/ 
--It returns a Leaf wich contans the move the algorithm has dicided to play and the utility of the end state that it thinks will result from perfect play after {depth} meny moves or a win/loss.
--The returned utility is usful for debuging. 
miniMaxLeaf :: [m] -> Integer -> ([m] -> [m]) -> ([m] -> Integer) -> Leaf m
miniMaxLeaf startMoves maxDepth sucFunc utilityFunc = alphaBetaFindLeaf maxDepth True startMoves (integerNegInfinity, integerInfinity)
    where alphaBetaFindLeaf depth isMax moves nodeState@(alpha, beta) -- the recursive implenentaion of Alpha Beta Pruning
            | length possibleMoves == 0 || depth == 0 = Leaf (moves !! (length startMoves)) (utilityFunc moves) -- Case for reaching the bottom of the tree and producing a leaf node.
            | isMax     = foldl  searchStepMaximizer firstChildLeaf possibleMoves 
            | not isMax = foldl searchStepBeta firstChildLeaf possibleMoves
            where firstChildLeaf = childLeaf (possibleMoves !! 0) nodeState -- Automaticaly evaluates the first child leaf to start the foldl. This is ok because the left most leaves can never be pruned.
                  possibleMoves = sucFunc moves
                  childLeaf move = alphaBetaFindLeaf (depth - 1) (not isMax) (moves ++ [move])
                  searchStepMaximizer valueLeaf move -- inside of the foldl for a maximizer layer.
                    | beta > utility valueLeaf  = if utility childValueLeaf > utility valueLeaf -- If the gard statment is false that is the equvilent of pruning the node.
                                                  then childValueLeaf 
                                                  else valueLeaf
                    |otherwise = valueLeaf 
                      where childValueLeaf = childLeaf move (utility valueLeaf, beta)
                  searchStepBeta valueLeaf move -- inside of the foldl for a minimizing layer
                    | alpha < utility valueLeaf = if utility childValueLeaf < utility valueLeaf
                                                  then childValueLeaf
                                                  else valueLeaf
                    | otherwise = valueLeaf
                      where childValueLeaf = childLeaf move (alpha, utility valueLeaf)

integerInfinity :: Integer
integerInfinity = 10000000
integerNegInfinity :: Integer
integerNegInfinity = -integerInfinity