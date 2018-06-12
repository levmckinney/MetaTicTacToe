module MetaTicTacToeBot 
(
    Bot
,   Leaf(..)
) where

import System.Random
import MetaTicTacToe
    

type Bot = (StdGen -> [Move] -> Maybe Move)

-- Represnets the leaf of the game tree. It holds the top move that spawned it and the utility of the final gamestate it represents.
data Leaf m = Leaf {move :: m, utility :: Integer} deriving (Show, Eq)
