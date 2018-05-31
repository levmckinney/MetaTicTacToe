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
                             then do stdget <- getStdGen
                                     gen <- newStdGen
                                     let resultString (b,r,d) = ("bot has won: " ++ show b ++ " Random has won: " ++ show r ++ " Draws: " ++ show d ++ " gen was: " ++ show stdget)
                                     putStrLn $ resultString winloss
                                     playGame (playFullRound winloss gen) (rounds - 1)
                             else return ()

playFullRound :: (Integer, Integer, Integer) -> StdGen -> (Integer, Integer, Integer)
playFullRound (iBotWins, iRandWins, iDraws) gen = (iBotWins + botXWins + botOWins, iRandWins + randXWins + randOWins, iDraws + draws + someThingWrong)
    where botXWins = if xBot == X then 1 else 0
          botOWins = if xRand == O then 1 else 0
          randXWins = if xRand == X then 1 else 0
          randOWins = if xBot == O then 1 else 0
          draws = (if xBot == Empty then 1 else 0) + (if xRand == Empty then 1 else 0)
          someThingWrong-- just for testing
             | botXWins + botOWins + randXWins + randOWins + draws == 2 = 0
             | otherwise = error $ "why is this happening xBot: " ++ show xBot ++ " xRand: " ++ show xRand ++ " botXwins: " ++ show botXWins ++ " botOWins: " ++ show botOWins ++ " randXWins " ++ show randXWins ++ " randOwins: " ++ show randOWins ++ " draws: " ++ show draws 
          xBot = playBotGameBotX gen
          xRand = playBotGameRandX gen




botDepth = 3

addMoves moves = fmap (\m -> moves ++ [m])

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
    