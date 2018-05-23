import qualified MetaTicTacToe as MT
import Data.List
import System.Environment  
import System.IO  
import System.IO.Error 
import MiniMaxBot

main = do putStrLn "PvP or PvAI"
          pvppvai <- getLine
          if pvppvai == "PvP" 
          then pvPGameLoop [] 
          else do putStrLn "would you like to play X or O"
                  xostr <- getLine
                  if xostr == "X" 
                  then pvAIGameLoop [] MT.X
                  else pvAIGameLoop [] MT.O

pvAIGameLoop moves xo = do let etherBoardError = MT.makeMoves MT.emptyMetaBoard moves
                           let board = case etherBoardError of Right b -> b
                                                               Left err -> MT.emptyMetaBoard      
                           let moveQuality = case etherBoardError of Right b -> MT.GoodMove
                                                                     Left err -> err
                           do if moveQuality == MT.GoodMove 
                                   then do if MT.turn board == xo 
                                           then if MT.toXO board == MT.Empty
                                                then do putStrLn ((show board) ++ "\n\n" ++ "It's " ++ (show $ MT.turn board) ++ "'s move.")
                                                        nextMove <- getMove 
                                                        pvAIGameLoop (moves ++ [nextMove]) xo
                                                else return ()
                                            else do putStrLn "How deep to seach (2-4 is recomended):"
                                                    depthStr <- getLine
                                                    let aiMove = botMove moves (read depthStr)
                                                    putStrLn ("Bot Thinking..." ++ (show aiMove))
                                                    pvAIGameLoop (moves ++ [aiMove]) xo
              
                                    else do print moveQuality
                                            pvAIGameLoop (init moves) xo
        
pvPGameLoop moves = do let etherBoardError = MT.makeMoves MT.emptyMetaBoard moves
                       let board = case etherBoardError of Right b -> b
                                                           Left err -> MT.emptyMetaBoard

                       let moveQuality = case etherBoardError of Right b -> MT.GoodMove
                                                                 Left err -> err
                       if moveQuality == MT.GoodMove 
                       then do if MT.toXO board == MT.Empty
                               then do putStrLn ((show board) ++ "\n\n" ++ "It's " ++ (show $ MT.turn board) ++ "'s move.")
                                       nextMove <- getMove 
                                       pvPGameLoop (moves ++ [nextMove])
                               else return ()
                       else do print moveQuality
                               pvPGameLoop (init moves)

getMove = do sMove <- getLine
             let lMove = map read (words sMove) :: [Int] -- find a way to handle this the error that can be trown
             return (lMove !! 0, lMove !! 1)

flipXO xo = if xo == MT.X then MT.O else MT.X