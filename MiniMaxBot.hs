
module MiniMaxBot 
(
    alphaBetaBotMove
,   alphaBetaBotLeaf
,   alphaBetaNoRandomBotMove
,   alphaBetaNoRandomBotLeaf
,   miniMaxBotMove
,   miniMaxBotLeaf
,   successor
) where

import qualified MiniMax as MM 
import qualified AlphaBeta as AB
import MetaTicTacToe
import System.Random
import MetaTicTacToeBot

type UtilityFunc = (XO -> StdGen -> [Move] -> Integer)

type MiniMaxAlgorithm = ([Move] -> Integer -> ([Move] -> [Move]) -> ([Move] -> Integer) -> Leaf Move) 

type BotLeaf = (StdGen -> [Move] -> Maybe (Leaf Move))

alphaBetaBotMove :: Integer -> StdGen -> [Move] -> Maybe Move
alphaBetaBotMove depth gen moves = getMove $ alphaBetaBotLeaf depth gen moves

alphaBetaBotLeaf :: Integer -> StdGen -> [Move] -> Maybe (Leaf Move) -- Returns Nothing if Game Alread won or noMore Valid Moves
alphaBetaBotLeaf = botLeafFactory AB.miniMaxLeaf randUtilityFunc

alphaBetaNoRandomBotMove :: Integer -> StdGen -> [Move] -> Maybe Move
alphaBetaNoRandomBotMove depth gen moves = getMove $ alphaBetaNoRandomBotLeaf depth gen moves

alphaBetaNoRandomBotLeaf :: Integer -> StdGen -> [Move] -> Maybe (Leaf Move) -- Returns Nothing if Game Alread won or noMore Valid Moves
alphaBetaNoRandomBotLeaf = botLeafFactory AB.miniMaxLeaf basicUtilityFunc

miniMaxBotMove :: Integer -> Bot
miniMaxBotMove depth gen moves = getMove $ miniMaxBotLeaf depth gen moves

miniMaxBotLeaf :: Integer -> BotLeaf -- Returns Nothing if Game Alread won or noMore Valid Moves
miniMaxBotLeaf = botLeafFactory MM.miniMaxLeaf randUtilityFunc

getMove :: Maybe (Leaf Move) -> Maybe Move
getMove maybeLeaf = move `fmap` maybeLeaf

botLeafFactory :: MiniMaxAlgorithm -> UtilityFunc -> (Integer -> BotLeaf)
botLeafFactory mmfunc util depth gen moves
    | successor moves == [] = Nothing
    | even(length moves)  = Just (mmfunc moves depth successor (util X gen))
    | odd(length moves) = Just (mmfunc moves depth successor (util O gen))


winValue = 1000
subBoardWinValue = 100
midCellValue = 10
maxMinNoise = 5

randUtilityFunc :: XO -> StdGen -> [Move] -> Integer
randUtilityFunc xo gen moves = (fst $ randomR (-maxMinNoise, maxMinNoise) (mkStdGen seed)) + basicUtilityFunc xo gen moves
  where seed = (fst $ random gen) + (sum $ map fst moves)


basicUtilityFunc :: XO -> StdGen -> [Move] -> Integer
basicUtilityFunc _ _ [] = 0
basicUtilityFunc xo gen moves = subBoardsU + gameWinU
  where mb = makeMoves' moves
        lsb i = look' mb i
        gsbU win
          | win == Empty = 0
          | win == xo = subBoardWinValue
          | otherwise = -subBoardWinValue
        subBoardsU = gsbU(lsb 0) + gsbU(lsb 1) + gsbU(lsb 2) + gsbU(lsb 3) + gsbU(lsb 4) + gsbU(lsb 5) + gsbU(lsb 6) + gsbU(lsb 7) + gsbU(lsb 8)
        gameWinU
          | toXO mb == Empty = 0
          | toXO mb == xo    = winValue
          | otherwise        = -winValue
        midControlU = cellU 0 + cellU 1 + cellU 2 + cellU 3 + cellU 4 + cellU 5 + cellU 6 + cellU 7 + cellU 8
          where midBoard = getSubBoard' mb 4
                cellU i = midXOU (look' midBoard i)
                midXOU ox
                  | ox == Empty = 0
                  | ox == xo    = midCellValue
                  | otherwise   = -midCellValue

          
          

successor :: [Move] -> [Move]
successor moves = possibleMoves . makeMoves' $ moves