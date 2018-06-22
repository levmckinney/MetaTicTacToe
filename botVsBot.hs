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
import MetaTicTacToeBot

main = do gen <- getStdGen
          playGame (0,0,0) 100

playGame :: (Integer, Integer, Integer) -> Integer -> IO ()
playGame winloss rounds = do if rounds > 0
                             then do gen <- newStdGen
                                     let resultString (b,r,d) = ("alphaBetaBotMove has won: " ++ show b ++ " miniMaxBotMove has won:  " ++ show r ++ " Draws: " ++ show d ++ " gen was: " ++ show gen)
                                     putStrLn $ resultString winloss
                                     playGame (playFullRound (alphaBetaBotMove 4) (miniMaxBotMove 4) winloss gen) (rounds - 1)
                             else return ()

playFullRound :: Bot -> Bot -> (Integer, Integer, Integer) -> StdGen -> (Integer, Integer, Integer)
playFullRound bot1 bot2 (iBot1Wins, iBot2Wins, iDraws) gen = (iBot1Wins + bot1Wins, iBot2Wins + bot2Wins, iDraws + draws)
    where gens = split gen
          bot1Wins = (if xBot1Result == X then 1 else 0) + (if xBot2Result == O then 1 else 0)
          bot2Wins = (if xBot2Result == X then 1 else 0) + (if xBot1Result == O then 1 else 0)
          draws = (if xBot1Result == Empty then 1 else 0) + (if xBot2Result == Empty then 1 else 0)
          xBot1Result = playBotGame bot1 bot2 (fst gens)
          xBot2Result = playBotGame bot2 bot1 (snd gens)

addMoves moves = fmap (\m -> moves ++ [m])
    
playBotGame :: Bot -> Bot -> StdGen -> XO
playBotGame xBot oBot initGen = nextRound [] (fst initGens) (snd initGens)
    where initGens = split initGen
          newGen gen = snd $ next gen
          nextRound moves genX genO
            | maybeBotXMoves == Nothing = toXO $ makeMoves' moves
            | maybeBotOMoves == Nothing = toXO $ makeMoves' moves
            | otherwise = nextRound botOMoves (newGen genX) (newGen genO)
            where maybeBotXMoves = addMoves moves $ xBot genX moves
                  botXMoves = fromJust maybeBotXMoves
                  maybeBotOMoves = addMoves botXMoves $ oBot genO botXMoves
                  botOMoves = fromJust maybeBotOMoves
