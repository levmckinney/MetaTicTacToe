module MetaTicTacToe
(  XO (..)
,  InvalideMove (..)
,  MetaBoard
,  SubBoard
,  Move (..)
,  getSubBoard
,  getSubBoard'
,  look
,  look'
,  makeMoves
,  turn
,  gameWonBy
,  emptyMetaBoard
,  possibleMoves
) where

type Cells b = (b, b, b, b, b, b, b, b, b)

type Move = (Integer, Integer)

data InvalideMove = OutOfRange | AllReadyOccupied | NotPlayble | GoodMove deriving (Show, Eq)

class XOable xoable where
    toXO :: xoable -> XO
    setXO :: xoable -> XO -> xoable

class (XOable board) => Board board where
    look :: (Integral a) => board -> a -> Maybe XO  
    draw :: (Integral a) => board -> XO -> a -> board
    
look' :: (Integral a, Board b) => b -> a -> XO 
look' b i = case look b i of Nothing -> error "out of range"
                             Just a -> a

data XO = X | O | Empty deriving Eq

instance XOable XO where
    toXO xo = xo
    setXO _ xo = xo
    
instance Show XO where
    show X = "X"
    show O = "O"
    show Empty = " "

data SubBoard = SubBoard {cells :: Cells XO, isPlayble :: Bool, wonBy :: XO}

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
                                       (sb, 0)               -> xoRow 0
                                       (_, 1)                -> line
                                       (sb, 2)               -> xoRow 1
                                       (_, 3)                -> line
                                       (sb, 4)               -> xoRow 2
    where xoRow r = (showLook sb (r*3 + 0)) ++ "|" ++ (showLook sb (r*3 + 1)) ++ "|" ++ (showLook sb (r*3 + 2))
          line = if isPlayble sb then take 5 (repeat '-') else makeLine 5

setPlayble :: SubBoard -> Bool -> SubBoard
setPlayble (SubBoard c _ w) b = SubBoard c b w 

instance Show SubBoard where
    show sb = (row 0) ++ "\n" ++ (row 1) ++ "\n" ++ (row 2) ++ "\n" ++ (row 3) ++ "\n" ++ (row 4) ++ "\n"
        where row = showSubBoardRow sb


data MetaBoard = MetaBoard {subBoards :: Cells SubBoard, turn :: XO, gameWonBy :: XO}

instance XOable MetaBoard where
    toXO = gameWonBy
    setXO (MetaBoard sbs t _) xo = MetaBoard sbs t xo 

instance Board MetaBoard where
    look (MetaBoard sbs _ _) i = fmap toXO (getCell sbs i)
    draw original@(MetaBoard sbs t w) xo i = case getCell sbs i of Nothing -> original 
                                                                   Just sb -> MetaBoard (setCell sbs (setXO sb xo) i) t w

instance Show MetaBoard where
    show mb
        | gameWonBy mb == X = "X Wins!!!"
        | gameWonBy mb == O = "O Wins!!!" 
        | gameWonBy mb == Empty = metaRow 0 ++ line ++ metaRow 1 ++ line ++ metaRow 2
        where line = makeLine 17 ++ "\n"
              metaRow mr = (row 0) ++ "\n" ++ (row 1) ++ "\n" ++ (row 2) ++ "\n" ++ (row 3) ++ "\n" ++ (row 4) ++ "\n"
                where row r = (sbRow (gsb 0) r) ++ "|" ++ (sbRow (gsb 1) r) ++ "|" ++ (sbRow (gsb 2) r) 
                        where sbRow = showSubBoardRow
                              gsb b = getSubBoard' mb (mr*3 + b)

applyToSubBoard' :: Integral a => MetaBoard -> (SubBoard -> SubBoard) -> a -> MetaBoard
applyToSubBoard' mb f i =  setSuboard mb (f (getSubBoard' mb i)) i 

mapToSubBoards :: MetaBoard -> (SubBoard -> SubBoard) -> MetaBoard
mapToSubBoards (MetaBoard sbs t w) f = MetaBoard (cellsMap sbs f) t w                                                                   

setTurn :: MetaBoard -> XO -> MetaBoard
setTurn (MetaBoard c _ w) xo = MetaBoard c xo w

getSubBoard :: (Integral a) => MetaBoard -> a -> Maybe SubBoard
getSubBoard (MetaBoard c _ _) i = getCell c i

getSubBoard' :: (Integral a) => MetaBoard -> a -> SubBoard
getSubBoard' mb i = case getSubBoard mb i of Nothing -> error "Out of Range"
                                             Just a -> a
setSuboard :: (Integral a) => MetaBoard -> SubBoard -> a -> MetaBoard
setSuboard (MetaBoard c t w) sb i = MetaBoard (setCell c sb i) t w

emptySubBoard :: SubBoard
emptySubBoard = SubBoard (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty) True Empty

emptyMetaBoard :: MetaBoard
emptyMetaBoard = MetaBoard (emptySubBoard, emptySubBoard, emptySubBoard, emptySubBoard, emptySubBoard, emptySubBoard, emptySubBoard, emptySubBoard, emptySubBoard) X Empty

showLook :: (Integral a, Board b) => b -> a -> String
showLook board i = case look board i of Nothing -> "Out of Range"
                                        Just xo -> show xo
makeLine :: Int -> String
makeLine i = take i (repeat '=')

cellsMap :: Cells a -> (a -> b) -> Cells b
cellsMap (a, b, c, d, e, f, g, h, i) fn = (fn a, fn b, fn c, fn d, fn e, fn f, fn g, fn h, fn i)

getCell :: (Integral a, XOable b) => Cells b -> a ->  Maybe b
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

setCell :: (Integral a, XOable b) => Cells b -> b -> a -> Cells b
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

makeMoves :: MetaBoard -> [Move] -> Either InvalideMove MetaBoard
makeMoves mb [] = Right mb
makeMoves mb ((move@(mm, sm)):remaining)
    | mm > 8 || mm < 0 ||sm > 8 || sm < 0 = Left OutOfRange
    | empty && isPlaybleSB = makeMoves setUpPlayble remaining
    | not empty = Left AllReadyOccupied
    | not isPlaybleSB = Left NotPlayble
    | otherwise = Right mb
    where empty = isEmpty mb move 
          isPlaybleSB = isPlayble (getSubBoard' mb mm)
          sb = (getSubBoard' mb mm)
          eval = metaWinner $ setSuboard mb (draw sb (turn mb) sm) mm
          switchTurn = if turn mb == X then setTurn eval O else setTurn eval X
          resetPlayble = mapToSubBoards switchTurn (flip setPlayble False)
          subBoardSentTo = getSubBoard' resetPlayble sm  
          setUpPlayble = if wonBy subBoardSentTo == Empty then applyToSubBoard' resetPlayble (flip setPlayble True) sm else mapToSubBoards resetPlayble (flip setPlayble True)

isEmpty :: MetaBoard -> Move -> Bool
isEmpty mb (mm, sm) = look' sb sm == Empty && wonBy sb == Empty
    where sb = getSubBoard' mb mm

possibleMoves :: MetaBoard -> [Move]
possibleMoves mb = [(mm, sm) | mm <- [0..8], sm <- [0..8], isPlayble $ getSubBoard' mb mm, isEmpty mb (mm, sm)]    

metaWinner :: MetaBoard -> MetaBoard
metaWinner mb = evalWinner (mapToSubBoards mb evalWinner)
    where evalWinner :: Board a => a -> a
          evalWinner board = setXO board $ winner board
            where   winner :: Board a => a -> XO
                    winner board 
                        | eq3NE (lb 0) (lb 1) (lb 2) = lb 0
                        | eq3NE (lb 3) (lb 4) (lb 5) = lb 3
                        | eq3NE (lb 6) (lb 7) (lb 8) = lb 6
                        | eq3NE (lb 0) (lb 3) (lb 6) = lb 3
                        | eq3NE (lb 1) (lb 4) (lb 7) = lb 1
                        | eq3NE (lb 2) (lb 5) (lb 8) = lb 2
                        | eq3NE (lb 0) (lb 4) (lb 8) = lb 0
                        | eq3NE (lb 6) (lb 4) (lb 2) = lb 6
                        | otherwise = Empty
                        where lb = look' board
                              eq3NE a b c = (a == b) && (b == c) && (a /= Empty)

shuck :: Either InvalideMove MetaBoard -> MetaBoard
shuck either = case either of Right mb -> mb
                              Left  err -> error "dont use shuck in non testing"
