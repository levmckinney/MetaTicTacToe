import MiniMax
import MetaTicTacToe

utility :: [Move] -> Integer
utility moves = gsb 0 + gsb 1 + gsb 2 + gsb 3 + gsb 4 + gsb 5 + gsb 6 + gsb 7 + gsb 8
    where gsb i = if look' (makeMoves' moves) i == X then 1 else 0

successor :: [Move] -> [Move]
successor moves = possibleMoves . makeMoves' $ moves

makeMoves' :: [Move] -> MetaBoard
makeMoves' moves = case makeMoves emptyMetaBoard moves of Left err -> error "the bot is making an invalid move"
                                                          Right mb -> mb