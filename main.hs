type Move = (Int, Int)

data XO = X | O | Empty deriving(Show)

class Board board where
    look :: (Integral a) => board -> a -> Maybe XO  
    draw :: (Integral a) => board -> XO -> a -> board

data SubBoard = SubBoard {cells :: (XO, XO, XO, XO, XO, XO, XO, XO, XO), isPlayble :: Bool, wonBy :: XO} deriving(Show)

instance Board SubBoard where
    look (SubBoard c _ _) = getCell c
    draw (SubBoard c p w) xo i = SubBoard (setCell c xo i) p w

data GameState = MetaBoard {subBoards :: (SubBoard, SubBoard, SubBoard, SubBoard, SubBoard, SubBoard, SubBoard, SubBoard, SubBoard), turn :: XO} | InvalidMoves deriving(Show)

instance Board GameState where
    look InvalidMoves _ = Nothing
    look (MetaBoard sbs t) i = fmap wonBy $ getCell sbs i
    draw InvalidMoves _ _= InvalidMoves
    draw (MetaBoard sbs t) xo i = case getCell sbs i of Nothing -> InvalidMoves 
                                                        Just sb -> MetaBoard (setCell sbs (newWiner sb xo) i) t
        where newWiner (SubBoard c p _) xo = SubBoard c p xo

getCell :: (Integral b) => (a, a, a, a, a, a, a, a, a) -> b ->  Maybe a
getCell (topLeft, _, _, _, _, _, _, _, _)  0 = Just topLeft
getCell (_, topMid, _, _, _, _, _, _, _)   1 = Just topMid
getCell (_, _, topRight, _, _, _, _, _, _) 2 = Just topRight 
getCell (_, _, _, midLeft, _, _, _, _, _)  3 = Just midLeft
getCell (_, _, _, _, midMid, _, _, _, _)   4 = Just midMid
getCell (_, _, _, _, _, midRight, _, _, _) 5 = Just midRight 
getCell (_, _, _, _, _, _, botLeft, _, _)  6 = Just botLeft 
getCell (_, _, _, _, _, _, _, botMid, _)   7 = Just botMid
getCell (_, _, _, _, _, _, _, _, botRight) 8 = Just botRight
getCell _ _ = Nothing

setCell :: (Integral b) => (a, a, a, a, a, a, a, a, a) -> a -> b -> (a, a, a, a, a, a, a, a, a)
setCell (_, b, c, d, e, f, g, h, i) topLeft  0 = (topLeft, b, c, d, e, f, g, h, i)
setCell (a, _, c, d, e, f, g, h, i) topMid   1 = (a, topMid,  c, d, e, f, g, h, i)
setCell (a, b, _, d, e, f, g, h, i) topRight 2 = (a, b, topRight, d, e, f, g, h, i)
setCell (a, b, c, _, e, f, g, h, i) midLeft  3 = (a, b, c, midLeft, e, f, g, h, i)
setCell (a, b, c, d, _, f, g, h, i) midMid   4 = (a, b, c, d, midMid, f, g, h, i)
setCell (a, b, c, d, e, _, g, h, i) midRight 5 = (a, b, c, d, e, midRight, g, h, i)
setCell (a, b, c, d, e, f, _, h, i) botLeft  6 = (a, b, c, d, e, f, botLeft, h, i)
setCell (a, b, c, d, e, f, g, _, i) botMid   7 = (a, b, c, d, e, f, g, botMid, i)
setCell (a, b, c, d, e, f, g, h, _) botRight 8 = (a, b, c, d, e, f, g, h, botRight)
setCell x _ _ = x