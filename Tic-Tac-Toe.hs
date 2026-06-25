import Data.List (intercalate, transpose)
import Text.Read (readMaybe)

data Cell = Empty | X | O deriving (Eq)
type Board = [[Cell]]
data Player = PlayerX | PlayerO deriving (Eq, Show)

instance Show Cell where
    show Empty = " "
    show X     = "X"
    show O     = "O"

emptyBoard :: Board
emptyBoard = replicate 3 (replicate 3 Empty)

checkWin :: Cell -> Board -> Bool
checkWin cell b = any allCell rows || any allCell cols || any allCell diags
  where
    rows  = b
    cols  = transpose b
    diags = [[b!!0!!0, b!!1!!1, b!!2!!2], [b!!0!!2, b!!1!!1, b!!2!!0]]
    allCell line = all (== cell) line

checkDraw :: Board -> Bool
checkDraw board = all (notElem Empty) board

updateBoard :: Board -> (Int, Int) -> Cell -> Board
updateBoard board (r, c) cell =
    take r board ++ [take c (board !! r) ++ [cell] ++ drop (c + 1) (board !! r)] ++ drop (r + 1) board

parseAllCoords :: String -> [(Int, Int)]
parseAllCoords input = pairs (map readMaybe (words input))
  where
    pairs (Just r : Just c : xs) = (r, c) : pairs xs
    pairs _                      = []

runGame :: Board -> Player -> [(Int, Int)] -> String
runGame board player [] = "Game ended: No more moves provided.\n"
runGame board player ((r, c):moves)
    | r < 0 || r >= 3 || c < 0 || c >= 3 = "Invalid bounds!\n" ++ runGame board player moves
    | board !! r !! c /= Empty           = "Cell taken!\n" ++ runGame board player moves
    | otherwise =
        let currentCell = if player == PlayerX then X else O
            newBoard    = updateBoard board (r, c) currentCell
            nextPlayer  = if player == PlayerX then PlayerO else PlayerX
            msg         = "Player " ++ show player ++ " moves to (" ++ show r ++ "," ++ show c ++ ")\n"
        in if checkWin currentCell newBoard
              then msg ++ "Player " ++ show player ++ " wins!\n"
              else if checkDraw newBoard
                      then msg ++ "It's a draw!\n"
                      else msg ++ runGame newBoard nextPlayer moves

main :: IO ()
main = do
    content <- getContents
    let moves = parseAllCoords content
    putStr (runGame emptyBoard PlayerX moves)
