import MetaTicTacToe
import Data.List
import System.Environment  
import System.IO  
import System.IO.Error 
import MiniMaxBot
import RandomBot
import AlphaBeta
import System.Random

main = do gen <- getStdGen
          playGame (0,0,0) 100

playGame :: (Integer, Integer, Integer) -> Integer -> IO ()
playGame winloss rounds = do if rounds > 0 
                             then do gen <- newStdGen
                                     let resultString (b,r,d) = ("bot has won: " ++ show b ++ " Random has won: " ++ show r ++ " Draws: " ++ show d ++ " gen was: " ++ show gen)
                                     putStrLn $ resultString winloss
                                     playGame (playFullRound winloss gen) (rounds - 1)
                             else return ()

playFullRound :: (Integer, Integer, Integer) -> StdGen -> (Integer, Integer, Integer)
playFullRound (iBotWins, iDraws, iRandWins) gen = (iBotWins + botXWins + botOWins, iRandWins + randXwins + randOwins, iDraws + draws)
    where botXWins = if xBot == X then 1 else 0
          botOWins = if xRand == O then 1 else 0
          randXwins = if xRand == X then 1 else 0
          randOwins = if xBot == O then 1 else 0
          draws = if xBot == Empty then 1 else 0 + if xRand == Empty then 1 else 0
          xBot = playBotGameBotX gen
          xRand = playBotGameRandX gen




addMoves moves = fmap (\m -> moves ++ [m])


botDepth = 3

playBotGameBotX :: StdGen -> XO
playBotGameBotX startGen = nextRoud startGen []
    where nextRoud gen moves
            | maybeBotMakeMove == Nothing = toXO $ makeMoves' moves
            | maybeRandMakeMove == Nothing = toXO $ makeMoves' moves
            | otherwise = nextRoud (snd $ (random gen :: (Integer ,StdGen))) randMakeMove
            where maybeBotMakeMove = addMoves moves $ botMove moves botDepth
                  botMakeMove = case maybeBotMakeMove of Just m -> m
                  maybeRandMakeMove = addMoves botMakeMove $ extraRandomMove gen botMakeMove
                  randMakeMove = case maybeRandMakeMove of Just m -> m

playBotGameRandX :: StdGen -> XO
playBotGameRandX startGen = nextRoud startGen []
    where nextRoud gen moves
            | maybeRandMakeMove == Nothing = toXO $ makeMoves' moves
            | maybeBotMakeMove == Nothing = toXO $ makeMoves' moves
            | otherwise = nextRoud ((snd $ (random gen :: (Integer ,StdGen)))) botMakeMove
            where maybeRandMakeMove = addMoves moves $ extraRandomMove gen moves
                  randMakeMove = case maybeRandMakeMove of Just m -> m
                  maybeBotMakeMove = addMoves randMakeMove $ botMove randMakeMove botDepth
                  botMakeMove = case maybeBotMakeMove of Just m -> m
    