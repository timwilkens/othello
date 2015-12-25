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
  show (Board t cells) = (show t) ++ "\n" ++ header ++ "\n" ++ (unlines cellRows)
    where header = "  a b c d e f g h"
          cellRows = map (\(x,y) -> x ++ " " ++ y) $
                       zip (map show [1..8]) $
                         (map (intercalate " ") $
                           chunksOf 8 $ map show cells)

newBoard :: Board
newBoard = Board BlackTurn startCells

startCells :: Cells
startCells = headAndTail ++ [White,Black] ++ (replicate 6 Empty) ++ [Black,White] ++ headAndTail
 where headAndTail = replicate 27 Empty

type Row = Int
type Col = Int
data CellLocation = CellLocation Col Row
  deriving (Show)

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
{-
  | n <= 8 = (CellLocation n 1)
  | n <= 16 = (CellLocation (n-8) 2)
  | n <= 24 = (CellLocation (n-16) 3)
  | n <= 32 = (CellLocation (n-24) 4)
  | n <= 40 = (CellLocation (n-32) 5)
  | n <= 48 = (CellLocation (n-40) 6)
  | n <= 56 = (CellLocation (n-48) 7)
  | n <= 64 = (CellLocation (n-56) 8)
  -}

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

lineColors :: [[CellLocation]] -> Cells -> [[(CellLocation, Cell)]]
lineColors lines cells = map (\x -> map (\p -> (p, cellColor p)) x) lines
  where cellColor c = cells !! ((locationToIndex c) - 1)

-- Don't include the passed in origin in the flip list
cellsToFlip :: Cells -> Cell -> CellLocation -> [CellLocation]
cellsToFlip cells color origin = if (length chain) == 0
                                    then []
                                    else concat $ map (map fst) chain
  where lineWithColors = lineColors (allLinesFromCell cells origin) cells
        chain = map fromJust $ filter (not . null) $ map (\x -> flipChain x color) lineWithColors

flipChain :: [(CellLocation, Cell)] -> Cell -> Maybe [(CellLocation, Cell)]
flipChain lineCells color = if (length enemies) < 1
                               then Nothing
                               else if (length friendStart) == 0 || (snd $ head friendStart) /= color
                                   then Nothing
                                   else Just enemies
  where enemyColor = invertCell color
        enemies = takeWhile (\x -> (snd x) == enemyColor) lineCells
        friendStart = dropWhile (\x -> (snd x) == enemyColor) lineCells

emptyCells :: Cells -> [CellLocation]
emptyCells cells = map indexToLocation $ map snd $ filter (\x -> isEmpty (fst x)) $ zip cells [0..]

hasMove :: Turn -> Cells -> Bool
hasMove t cells = or $ map (\x -> length (cellsToFlip cells color x) > 0) empties
  where empties = emptyCells cells
        color = turnToCell t

setCells :: Cells -> Cell -> [CellLocation] -> Cells
setCells cells _ [] = cells
setCells cells color (x:xs) = setCells newCells color xs
  where index = locationToIndex x
        newCells = (take (index-1) cells) ++ (color:(drop index cells))

-- Check if move is valid based on neighboring cells
-- This function should manipulate the cells but *not*
-- the turn
doMove :: Board -> Cell -> CellLocation -> Maybe Board
doMove (Board t cells) play loc = if cellAvailable
                                     then if (length toFlip) == 0
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
