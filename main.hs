type Move = (Int, Int)

class XOable xoable where
    toXO :: xoable -> XO
    setXO :: xoable -> XO -> xoable

class (XOable board) => Board board where
    look :: (Integral a) => board -> a -> Maybe XO  
    draw :: (Integral a) => board -> XO -> a -> board

data XO = X | O | Empty

instance XOable XO where
    toXO xo = xo
    setXO _ xo = xo
    
instance Show XO where
    show X = "X"
    show O = "O"
    show Empty = " "

data SubBoard = SubBoard {cells :: (XO, XO, XO, XO, XO, XO, XO, XO, XO), isPlayble :: Bool, wonBy :: XO}

instance XOable SubBoard where
    toXO = wonBy
    setXO (SubBoard cs p _) xo = SubBoard cs p xo

instance Board SubBoard where
    look (SubBoard cs _ _) i = getCell cs i
    draw (SubBoard cs p w) xo i = SubBoard (setCell cs xo i) p w

showSubBoardRow :: (Integral a) => SubBoard -> a -> String
showSubBoardRow sb i = case (sb, i) of ((SubBoard _ _ X), 0) -> "\\   /"
                                       ((SubBoard _ _ X), 1) -> " \\ / "
                                       ((SubBoard _ _ X), 2) -> "  X  "
                                       ((SubBoard _ _ X), 3) -> " / \\ "
                                       ((SubBoard _ _ X), 4) -> "/   \\"
                                       ((SubBoard _ _ O), 0) -> " --- "
                                       ((SubBoard _ _ O), 1) -> "/   \\"
                                       ((SubBoard _ _ O), 2) -> "|   |"
                                       ((SubBoard _ _ O), 3) -> "\\   /"
                                       ((SubBoard _ _ O), 4) -> " --- "
                                       (sb, 0)               -> xoRow sb 0
                                       (_, 1)                -> line
                                       (sb, 2)               -> xoRow sb 1
                                       (_, 3)                -> line
                                       (sb, 4)               -> xoRow sb 2
    where xoRow sb r = (showLook sb (r*3 + 0)) ++ "|" ++ (showLook sb (r*3 + 1)) ++ "|" ++ (showLook sb (r*3 + 2))
          line = makeLine 5

instance Show SubBoard where
    show sb = (row 0) ++ "\n" ++ (row 1) ++ "\n" ++ (row 2) ++ "\n" ++ (row 3) ++ "\n" ++ (row 4) ++ "\n"
        where row = showSubBoardRow sb


data MetaBoard = MetaBoard {subBoards :: (SubBoard, SubBoard, SubBoard, SubBoard, SubBoard, SubBoard, SubBoard, SubBoard, SubBoard), turn :: XO, gameWonBy :: XO}

instance XOable MetaBoard where
    toXO = gameWonBy
    setXO (MetaBoard sbs t _) xo = MetaBoard sbs t xo 

instance Board MetaBoard where
    look (MetaBoard sbs _ _) i = fmap toXO (getCell sbs i)
    draw original@(MetaBoard sbs t w) xo i = case getCell sbs i of Nothing -> original 
                                                                   Just sb -> MetaBoard (setCell sbs (setXO sb xo) i) t w

instance Show MetaBoard where
    show mb = metaRow mb 0 ++ line ++ metaRow mb 1 ++ line ++ metaRow mb 2
        where line = makeLine 17 ++ "\n"
              metaRow mb mr = (row mb mr 0) ++ "\n" ++ (row mb mr 1) ++ "\n" ++ (row mb mr 2) ++ "\n" ++ (row mb mr 3) ++ "\n" ++ (row mb mr 4) ++ "\n"
                where row mb mr r = (sbRow (gsb mb mr 0) r) ++ "|" ++ (sbRow (gsb mb mr 1) r) ++ "|" ++ (sbRow (gsb mb mr 2) r) 
                        where sbRow = showSubBoardRow
                              gsb mb mr b = case getSubBoard mb (mr*3 + b) of Just sb -> sb
                                                                              Nothing -> emptySubBoard
         

getSubBoard :: (Integral a) => MetaBoard -> a -> Maybe SubBoard
getSubBoard (MetaBoard c _ _) i = getCell c i

emptySubBoard :: SubBoard
emptySubBoard = SubBoard (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty) False Empty

emptyMetaBoard :: MetaBoard
emptyMetaBoard = MetaBoard (emptySubBoard, emptySubBoard, emptySubBoard, emptySubBoard, emptySubBoard, emptySubBoard, emptySubBoard, emptySubBoard, emptySubBoard) X Empty

showLook :: (Integral a, Board b) => b -> a -> String
showLook board i = case look board i of Nothing -> "Out of Range"
                                        Just xo -> show xo
makeLine :: Int -> String
makeLine i = take i (repeat '=')

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