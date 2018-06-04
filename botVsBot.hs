import MetaTicTacToe
import Data.List
import System.Environment  
import System.IO  
import System.IO.Error 
import MiniMaxBot
import RandomBot
import AlphaBeta
import System.Random
import Data.Maybe

type Bot = ([Move] -> Maybe Move)

main = do gen <- getStdGen
          playGame (0,0,0) 100

playGame :: (Integer, Integer, Integer) -> Integer -> IO ()
playGame winloss rounds = do if rounds > 0
                             then do gen <- newStdGen
                                     let resultString (b,r,d) = ("bot has won: " ++ show b ++ " Random has won: " ++ show r ++ " Draws: " ++ show d ++ " gen was: " ++ show gen)
                                     putStrLn $ resultString winloss
                                     playGame (playFullRound randomMove (flip alphBetaLeaf 3) winloss gen) (rounds - 1)
                             else return ()

playFullRound :: Bot -> Bot -> (Integer, Integer, Integer) -> StdGen -> (Integer, Integer, Integer)
playFullRound bot1 bot2 (iBot1Wins, iBot2Wins, iDraws) gen = (iBot1Wins + bot1Wins, iBot2Wins + bot2Wins, iDraws + draws)
    where bot1Wins = (if xBot1Result == X then 1 else 0) + (if xBot2Result == O then 1 else 0)
          bot2Wins = (if xBot2Result == X then 1 else 0) + (if xBot1Result == O then 1 else 0)
          draws = (if xBot1Result == Empty then 1 else 0) + (if xBot2Result == Empty then 1 else 0)
          xBot1Result = playBotGame bot1 bot2
          xBot2Result = playBotGame bot2 bot1

addMoves moves = fmap (\m -> moves ++ [m])
    
playBotGame :: Bot -> Bot -> XO
playBotGame xBot oBot = nextRound []
    where nextRound moves
            | maybeBotXMoves == Nothing = toXO $ makeMoves' moves
            | maybeBotOMoves == Nothing = toXO $ makeMoves' moves
            | otherwise = nextRound botOMoves
            where maybeBotXMoves = addMoves moves $ xBot moves
                  botXMoves = fromJust maybeBotXMoves
                  maybeBotOMoves = addMoves moves $ oBot moves
                  botOMoves = fromJust maybeBotOMoves
