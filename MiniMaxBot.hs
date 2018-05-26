
module MiniMaxBot 
(
    botMove
,   botLeaf 
) where

import qualified MiniMax as MM 
import qualified AlphaBeta as AB
import MetaTicTacToe
import System.Random

botMove :: [Move] -> Integer -> Move
botMove moves depth
    | even(length moves)  = AB.miniMax moves depth successor (utility X)
    | odd(length moves) = AB.miniMax moves depth successor (utility O)

botLeaf :: [Move] -> Integer -> AB.Leaf Move
botLeaf moves depth
    | even(length moves)  = AB.miniMaxLeaf moves depth successor (utility X)
    | odd(length moves) = AB.miniMaxLeaf moves depth successor (utility O)


subBoardWinValue = 100
midCellValue = 10
maxMinNoise = 5

utility :: XO -> [Move] -> Integer
utility _ [] = 0
utility xo moves = subBoardsU + gameWinU + randomOffset
    where randomOffset = fst $ randomR (-maxMinNoise, maxMinNoise) (mkStdGen seed)
            where seed = sum (map fst moves)
          mb = makeMoves' moves
          lsb i = look' mb i
          gsbU win
            | win == Empty = 0
            | win == xo = subBoardWinValue
            | otherwise = -subBoardWinValue
          subBoardsU = gsbU(lsb 0) + gsbU(lsb 1) + gsbU(lsb 2) + gsbU(lsb 3) + gsbU(lsb 4) + gsbU(lsb 5) + gsbU(lsb 6) + gsbU(lsb 7) + gsbU(lsb 8)
          gameWinU
            | toXO mb == Empty = 0
            | toXO mb == xo    = AB.integerInfinity
            | otherwise        = AB.integerNegInfinity
          midControlU = cellU 0 + cellU 1 + cellU 2 + cellU 3 + cellU 4 + cellU 5 + cellU 6 + cellU 7 + cellU 8
            where midBoard = getSubBoard' mb 4
                  cellU i = midXOU (look' midBoard i)
                  midXOU ox
                    | ox == Empty = 0
                    | ox == xo    = midCellValue
                    | otherwise   = -midCellValue

          
          

successor :: [Move] -> [Move]
successor moves = possibleMoves . makeMoves' $ moves

makeMoves' :: [Move] -> MetaBoard
makeMoves' moves = case makeMoves emptyMetaBoard moves of Left err -> error "the bot is making an invalid move"
                                                          Right mb -> mb