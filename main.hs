import qualified MetaTicTacToe as MT
import Data.List
import System.Environment  
import System.IO  
import System.IO.Error  

main = gameLoop []
      
gameLoop moves = do let etherBoardError = MT.makeMoves MT.emptyMetaBoard moves
                    let board = case etherBoardError of Right b -> b
                                                        Left err -> MT.emptyMetaBoard

                    let moveQuality = case etherBoardError of Right b -> MT.GoodMove
                                                              Left err -> err
                    if moveQuality == MT.GoodMove 
                    then do if MT.gameWonBy board == MT.Empty
                            then do putStrLn ((show board) ++ "\n\n" ++ "It's " ++ (show $ MT.turn board) ++ "'s move.")
                                    nextMove <- getMove 
                                    gameLoop (moves ++ [nextMove])
                            else return ()
                    else do print moveQuality
                            gameLoop (init moves)

getMove = do sMove <- getLine
             let lMove = map read (words sMove) :: [Int] -- find a way to handle this the error that can be trown
             return (lMove !! 0, lMove !! 1)