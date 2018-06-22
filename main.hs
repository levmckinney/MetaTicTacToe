import qualified MetaTicTacToe as MT
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


main = do putStrLn "Moves should be of the form: \n1 2 \nor \n0 8 \nBoth numbers should be in the range 0 to 8. \nTry making some moves in PvP to get the hang of it. :D \n"
          putStrLn "PvP or PvAI"
          pvppvai <- getLine
          if pvppvai == "PvP" 
          then pvPGameLoop [] 
          else do putStrLn "would you like to play X or O"
                  xostr <- getLine
                  if xostr == "X" 
                  then pvAIGameLoop [] MT.X
                  else pvAIGameLoop [] MT.O

pvAIGameLoop moves xo = do let etherBoardORError = MT.makeMoves MT.emptyMetaBoard moves
                           let board = case etherBoardORError of Right b -> b
                                                                 Left err -> MT.emptyMetaBoard      
                           let moveQuality = case etherBoardORError of Right b -> MT.GoodMove
                                                                       Left err -> err
                           do if moveQuality == MT.GoodMove 
                                   then do if MT.turn board == xo 
                                           then if MT.toXO board == MT.Empty
                                                then do putStrLn ((show board) ++ "\n\n" ++ "It's " ++ (show $ MT.turn board) ++ "'s move.")
                                                        maybeNexMove <- getMove 
                                                        if maybeNexMove /= Nothing
                                                        then pvAIGameLoop (moves ++ [fromJust maybeNexMove]) xo
                                                        else do putStrLn  $ "That input was invalid. All moves must be of the form:   Num Num   \nExample:0 8\n" ++ show moves -- this is for testing proboply should remove at some point
                                                                restartMove
                                                else return ()
                                            else do putStrLn "How deep to search (2-3 is recomended):"
                                                    depthStr <- getLine
                                                    gen <- newStdGen
                                                    let readDepth = reads depthStr :: [(Integer, String)]
                                                        checkDepth [] = Nothing
                                                        checkDepth [(d,_)] = Just d
                                                        maybeDepth = checkDepth readDepth
                                                    if maybeDepth /= Nothing
                                                    then do let maybeAiLeaf = alphaBetaBotLeaf (fromJust maybeDepth) gen moves -- change bot here
                                                            if maybeAiLeaf == Nothing
                                                            then print board  
                                                            else do let aiLeaf = case maybeAiLeaf of Just a -> a
                                                                        aiMove = move aiLeaf
                                                                        aiPFU = utility aiLeaf
                                                                    putStrLn ("Bot Thinking..." ++ (show aiMove) ++ " PFU: " ++ (show aiPFU))
                                                                    pvAIGameLoop (moves ++ [aiMove]) xo
                                                    else do putStrLn "Depth is a number. Just type a number. It should be in the range of 2-3 for the best results."
                                                            restartMove
                                    else do print moveQuality
                                            restartMove
        where restartMove = if moves /= [] then pvAIGameLoop (init moves) xo else pvAIGameLoop [] xo

pvPGameLoop moves = do let etherBoardORError = MT.makeMoves MT.emptyMetaBoard moves
                       let board = case etherBoardORError of Right b -> b
                                                             Left err -> MT.emptyMetaBoard

                       let moveQuality = case etherBoardORError of Right b -> MT.GoodMove
                                                                   Left err -> err
                       if moveQuality == MT.GoodMove 
                       then do if MT.toXO board == MT.Empty
                               then do putStrLn ((show board) ++ "\n\n" ++ "It's " ++ (show $ MT.turn board) ++ "'s move.")
                                       maybeNexMove <- getMove 
                                       if maybeNexMove /= Nothing 
                                       then pvPGameLoop (moves ++ [fromJust maybeNexMove])
                                       else do putStrLn "That input was invalid. All moves must be of the form:   Num Num   \nExample:1 2"
                                               restartMove
                               else return ()
                       else do print moveQuality
                               restartMove
        where restartMove = if moves /= [] then pvPGameLoop (init moves) else pvPGameLoop []

getMove = do sMove <- getLine
             return $ prossesMove sMove

prossesMove :: String -> Maybe MT.Move
prossesMove sMove = checkNums $ map (reads :: String -> [(Int,String)]) (words sMove)
        where checkNums [] = Nothing
              checkNums (_:[]) = Nothing
              checkNums ([]:_:[]) = Nothing
              checkNums (_:[]:[]) = Nothing
              checkNums ([(mm, _)]:[(sm, _)]:[]) = Just (mm, sm)