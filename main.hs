type Move = (Int, Int)


class XOable xoable where
    toXO :: xoable -> XO
    setXO :: xoable -> XO -> xoable

    --look should have b be restricted to XOable
class (XOable board) => Board board where
    look :: (Integral a) => board -> a -> Maybe XO  
    draw :: (Integral a) => board -> XO -> a -> board

data XO = X | O | Empty deriving Show

instance XOable XO where
    toXO xo = xo
    setXO _ xo = xo

data SubBoard = SubBoard {cells :: (XO, XO, XO, XO, XO, XO, XO, XO, XO), isPlayble :: Bool, wonBy :: XO} deriving Show

instance XOable SubBoard where
    toXO = wonBy
    setXO (SubBoard cs p _) xo = SubBoard cs p xo

instance Board SubBoard where
    look (SubBoard cs _ _) i = getCell cs i
    draw (SubBoard cs p w) xo i = SubBoard (setCell cs xo i) p w

data GameState = MetaBoard {subBoards :: (SubBoard, SubBoard, SubBoard, SubBoard, SubBoard, SubBoard, SubBoard, SubBoard, SubBoard), turn :: XO, gameWonBy :: XO} | InvalidMoves deriving Show

instance XOable GameState where
    toXO = gameWonBy
    setXO (MetaBoard sbs t _) xo = MetaBoard sbs t xo 

instance Board GameState where
    look InvalidMoves _ = Nothing
    look (MetaBoard sbs _ _) i = fmap toXO (getCell sbs i)
    draw InvalidMoves _ _= InvalidMoves
    draw (MetaBoard sbs t w) xo i = case getCell sbs i of Nothing -> InvalidMoves 
                                                          Just sb -> MetaBoard (setCell sbs (setXO sb xo) i) t w
emptySubBoard :: SubBoard
emptySubBoard = SubBoard (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty) False Empty

emptyMetaBoard :: GameState
emptyMetaBoard = MetaBoard (emptySubBoard, emptySubBoard, emptySubBoard, emptySubBoard, emptySubBoard, emptySubBoard, emptySubBoard, emptySubBoard, emptySubBoard) X Empty

getCell :: (Integral a, XOable b) => (b, b, b, b, b, b, b, b, b) -> a ->  Maybe b
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

setCell :: (Integral a, XOable b) => (b, b, b, b, b, b, b, b, b) -> b -> a -> (b, b, b, b, b, b, b, b, b)
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