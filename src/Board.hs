module Board where

import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe

data Cell = White
          | Black
          | Empty
  deriving (Eq)

instance Show Cell where
  show White = "o"
  show Black = "●"
  show Empty = " "

isEmpty :: Cell -> Bool
isEmpty Empty = True
isEmpty _ = False

isWhite :: Cell -> Bool
isWhite White = True
isWhite _ = False

isBlack :: Cell -> Bool
isBlack Black = True
isBlack _ = False

invertCell :: Cell -> Cell
invertCell White = Black
invertCell Black = White
invertCell Empty = Empty

data Turn = WhiteTurn | BlackTurn
  deriving (Eq)

instance Show Turn where
  show WhiteTurn = "[" ++ (show White) ++ "]"
  show BlackTurn = "[" ++ (show Black) ++ "]"

turnToCell :: Turn -> Cell
turnToCell WhiteTurn = White
turnToCell BlackTurn = Black

invertTurn :: Turn -> Turn
invertTurn WhiteTurn = BlackTurn
invertTurn BlackTurn = WhiteTurn

type Cells = [Cell]
data Board = Board Turn Cells

instance Show Board where
  show (Board t cells) = unlines [(show t),header,rowSep,unlines $ map addSep cellRows]
    where header = "  a b c d e f g h"
          cellRows = map (\(x,y) -> x ++ "|" ++ y ++ "|") $
                       zip (map show [1..8]) $
                         (map (intercalate "|") $
                           chunksOf 8 $ map show cells)
          rowSep = ' ' : (take 17 $ merge (repeat '+') (repeat '-'))
          addSep x = x ++ "\n" ++ rowSep

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

newBoard :: Board
newBoard = Board BlackTurn startCells

startCells :: Cells
startCells = headAndTail ++ [White,Black] ++ (replicate 6 Empty) ++ [Black,White] ++ headAndTail
 where headAndTail = replicate 27 Empty

type Row = Int
type Col = Int
data CellLocation = CellLocation Col Row
  deriving (Eq)

instance Show CellLocation where
  show (CellLocation col row) = (chr $ col + 96):show row

isCorner :: CellLocation -> Bool
isCorner (CellLocation 1 1) = True
isCorner (CellLocation 1 8) = True
isCorner (CellLocation 8 1) = True
isCorner (CellLocation 8 8) = True
isCorner _ = False

isEdge :: CellLocation -> Bool
isEdge (CellLocation col row)
  | col == 1 || col == 8 = True
  | row == 1 || row == 8 = True
  | otherwise = False

isXCell :: CellLocation -> Bool
isXCell c = c `elem` xCells
  where xCells = map fromJust $ map parseCellLocation cells
        cells = ["b1","b2","a2","g1","g2","h2","a7","b7","b8","h7","g7","g8"]

parseCellLocation :: String -> Maybe CellLocation
parseCellLocation (x:y:[])
  | (not $ x `elem` ['a'..'h']) || (not $ y `elem` ['1'..'8']) = Nothing
  | otherwise = Just $ CellLocation ((ord x) - 96) (digitToInt y)
parseCellLocation _ = Nothing

locationToIndex :: CellLocation -> Int
locationToIndex (CellLocation c r) = (8 * (r-1)) + c

indexToLocation :: Int -> CellLocation
indexToLocation n = (CellLocation col row)
  where col = (n `mod` 8) + 1
        row = (n `div` 8) + 1

data Direction = U | D | L | R | UL | UR | DL | DR

cellNeighbor :: CellLocation -> Direction -> Maybe CellLocation
cellNeighbor (CellLocation c r) d = case d of
                                      U -> case r of
                                             1 -> Nothing
                                             _ -> Just (CellLocation c (r-1))
                                      D -> case r of
                                             8 -> Nothing
                                             _ -> Just (CellLocation c (r+1))
                                      L -> case c of
                                             1 -> Nothing
                                             _ -> Just (CellLocation (c-1) r)
                                      R -> case c of
                                             8 -> Nothing
                                             _ -> Just (CellLocation (c+1) r)
                                      UL -> if (c-1) == 0 || (r-1) == 0
                                               then Nothing
                                               else Just (CellLocation (c-1) (r-1))
                                      UR -> if (c+1) == 9 || (r-1) == 0
                                               then Nothing
                                               else Just (CellLocation (c+1) (r-1))
                                      DL -> if (c-1) == 0 || (r+1) == 9
                                               then Nothing
                                               else Just (CellLocation (c-1) (r+1))
                                      DR -> if (c+1) == 9 || (r+1) == 9
                                               then Nothing
                                               else Just (CellLocation (c+1) (r+1))

allLinesFromCell :: Cells -> CellLocation -> [[CellLocation]]
-- We are guaranteed to have at least one cell (origin) in each list so
-- tail is safe to use
allLinesFromCell cells origin = filter (not . null) $ map tail unwrappedLines
  where unwrappedLines = map (map fromJust) $ map (takeWhile isJust) allLines
        allLines = map (\x -> iterate (unwrappedNeighbor x) (Just origin)) [U,D,R,L,UR,UL,DR,DL]
        unwrappedNeighbor d point = case point of
                                    Nothing -> Nothing
                                    (Just c) -> cellNeighbor c d

lineColors :: [[CellLocation]] -> Cells -> [[(CellLocation,Cell)]]
lineColors lines cells = map (\x -> map (\p -> (p,cellColor p)) x) lines
  where cellColor c = cells !! ((locationToIndex c) - 1)

-- Don't include the passed in origin in the flip list
cellsToFlip :: Cells -> Cell -> CellLocation -> [CellLocation]
cellsToFlip cells color origin = if null chain
                                    then []
                                    else concat $ map (map fst) chain
  where lineWithColors = lineColors (allLinesFromCell cells origin) cells
        chain = map fromJust $ filter (not . null) $ map (\x -> flipChain x color) lineWithColors

flipChain :: [(CellLocation,Cell)] -> Cell -> Maybe [(CellLocation,Cell)]
flipChain lineCells color = if null enemies
                               then Nothing
                               else if null friendStart || (snd $ head friendStart) /= color
                                   then Nothing
                                   else Just enemies
  where enemyColor = invertCell color
        enemies = takeWhile (\x -> (snd x) == enemyColor) lineCells
        friendStart = dropWhile (\x -> (snd x) == enemyColor) lineCells

emptyCells :: Cells -> [CellLocation]
emptyCells cells = map indexToLocation $ map snd $ filter (\x -> isEmpty (fst x)) $ zip cells [0..]

hasMove :: Board -> Bool
hasMove (Board t cells) = or $ map (\x -> not $ null (cellsToFlip cells color x)) empties
  where empties = emptyCells cells
        color = turnToCell t

availableMoves :: Board -> [([CellLocation],CellLocation)]
availableMoves (Board t cells) = filter (\x -> not $ null $ fst x) $ map (\x -> ((cellsToFlip cells color x), x)) empties
  where empties = emptyCells cells
        color = turnToCell t

sortPossible :: ([CellLocation],CellLocation) -> ([CellLocation],CellLocation) -> Ordering
sortPossible (x,_) (y,_)
  | xLen > yLen = GT
  | xLen < yLen = LT
  | otherwise = EQ
    where xLen = length x
          yLen = length y

chooseMove :: Board -> CellLocation
-- For now return the move that would flip the *least* enemy cells
-- Try to move in corners or edges and avoid placing disks
-- directly adjacent to the corners
chooseMove b@(Board turn cells) = if null sortedMoves
                                     then (CellLocation 1 1)
                                     else head $ head priority
  where moves = availableMoves b
        sortedMoves = map snd $ sortBy sortPossible moves
        corners = filter isCorner sortedMoves
        edges = filter (not.isXCell) $ filter isEdge sortedMoves
        movesWithoutX = filter (not.isXCell) sortedMoves
        priority = filter (not.null) [corners,edges,movesWithoutX,sortedMoves]

setCells :: Cells -> Cell -> [CellLocation] -> Cells
setCells cells _ [] = cells
setCells cells color (x:xs) = setCells newCells color xs
  where index = locationToIndex x
        newCells = (take (index-1) cells) ++ (color:(drop index cells))

doMove :: Board -> Cell -> CellLocation -> Maybe Board
doMove (Board t cells) play loc = if cellAvailable
                                     then if null toFlip
                                             then Nothing
                                             else Just (Board t flippedCells)
                                     else Nothing
  where cellIndex = (locationToIndex loc)
        cellAvailable = isEmpty $ cells !! (cellIndex-1)
        newCells = setCells cells play [loc]
        toFlip = cellsToFlip cells play loc
        flippedCells = setCells cells play (loc:toFlip)

determineWinner :: Board -> Maybe Turn
determineWinner (Board _ cells) = if whiteCount > blackCount
                                     then Just WhiteTurn
                                     else if whiteCount < blackCount
                                            then Just BlackTurn
                                            else Nothing
  where whiteCount = length $ filter isWhite cells
        blackCount = length $ filter isBlack cells
