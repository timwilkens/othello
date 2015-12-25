module Main where

import Control.Monad
import Data.Maybe

import Board

playGame :: Board -> IO Board
playGame board@(Board turn cells) = do
    putStrLn $ show board
    case length $ filter isEmpty cells of
      0 -> do
        return board
      _ -> do
        if not $ hasMove turn cells
           then do
             putStrLn $ "== Player has no moves! =="
             playGame (Board (invertTurn turn) cells)
           else do
             move <- getLine
             let moveLocation = parseCellLocation move
             case moveLocation of
               Nothing -> do
                 putStrLn $ "== Invalid move location =="
                 playGame board
               (Just moveToDo) -> do
                 let updatedBoard = doMove board (turnToCell turn) moveToDo
                 case updatedBoard of
                   Nothing -> do
                     putStrLn "== Invalid move =="
                     playGame board
                   (Just (Board _ cells)) -> do
                     playGame (Board (invertTurn turn) cells)

main :: IO ()
main = do
  finalBoard <- playGame newBoard
  case determineWinner finalBoard of
    Nothing -> putStrLn "=-=-=-= Tie =-=-=-="
    (Just WhiteTurn) ->
      putStrLn "=-=-=-= White Wins =-=-=-=-="
    (Just BlackTurn) -> do
      putStrLn "=-=-=-= Black Wins =-=-=-=-="
