module ChessBoard where

import Data.Array

data Piece = Empty | WhitePawn | BlackPawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)

type Board = Array (Int, Int) Piece

initialBoard :: Board
initialBoard = array ((0,0), (11,9)) $ concat
  [ [((i, j), startingPosition !! (i * 10 + j)) | j <- [0..9]] | i <- [0..11]
  ]
  where
    startingPosition =
      [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty
      , Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty
      , Empty, Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook, Empty
      , Empty, WhitePawn, WhitePawn, WhitePawn, WhitePawn, WhitePawn, WhitePawn, WhitePawn, WhitePawn, Empty
      , Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty
      , Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty
      , Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty
      , Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty
      , Empty, BlackPawn, BlackPawn, BlackPawn, BlackPawn, BlackPawn, BlackPawn, BlackPawn, BlackPawn, Empty
      , Empty, Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook, Empty
      , Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty
      , Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty
      ]

mailBoxes = [
     -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
     -1,  0,  1,  2,  3,  4,  5,  6,  7, -1,
     -1,  8,  9, 10, 11, 12, 13, 14, 15, -1,
     -1, 16, 17, 18, 19, 20, 21, 22, 23, -1,
     -1, 24, 25, 26, 27, 28, 29, 30, 31, -1,
     -1, 32, 33, 34, 35, 36, 37, 38, 39, -1,
     -1, 40, 41, 42, 43, 44, 45, 46, 47, -1,
     -1, 48, 49, 50, 51, 52, 53, 54, 55, -1,
     -1, 56, 57, 58, 59, 60, 61, 62, 63, -1,
     -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
     -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
  ]

-- Generate all legal moves for a pawn at a given square
whitePawnMoves :: (Int, Int) -> Board -> [(Int, Int)]
whitePawnMoves (rank, file) board =
  case board ! (rank, file) of
    WhitePawn ->
      let
        forwardMoves =
          filter (\(r, _) -> board ! (r, file) == Empty) [(rank + 1, file), (rank + 2, file)]

        captureMoves =
          filter (\(r, c) -> board ! (r, c) /= Empty) [(rank + 1, file - 1), (rank + 1, file + 1)]
      in
        forwardMoves ++ captureMoves
    _ -> []

-- Generate all legal moves for a pawn at a given square
blackPawnMoves :: (Int, Int) -> Board -> [(Int, Int)]
blackPawnMoves (rank, file) board =
  case board ! (rank, file) of
    BlackPawn ->
      let
        forwardMoves =
          filter (\(r, _) -> board ! (r, file) == Empty) [(rank - 1, file), (rank - 2, file)]

        captureMoves =
          filter (\(r, c) -> board ! (r, c) /= Empty) [(rank - 1, file + 1), (rank - 1, file - 1)]
      in
        forwardMoves ++ captureMoves
    _ -> []

-- Generate all legal moves for a knight at a given square
knightMoves :: (Int, Int) -> Board -> [(Int, Int)]
knightMoves (rank, file) board =
  case board ! (rank, file) of
    Knight ->
      let
        potentialMoves =
          [ (rank + 2, file + 1), (rank + 2, file - 1)
          , (rank - 2, file + 1), (rank - 2, file - 1)
          , (rank + 1, file + 2), (rank - 1, file + 2)
          , (rank + 1, file - 2), (rank - 1, file - 2)
          ]
        validMoves = filter (\(r, c) -> r >= 0 && r < 12 && c >= 0 && c < 10 && board ! (r, c) /= Empty && mailBoxes !! (r * 8 + c) == -1) potentialMoves
      in
        potentialMoves
    _ -> []

-- Generate all legal moves for the entire board
allMoves :: Board -> [((Int, Int), [(Int, Int)])]
allMoves board =
  [((i, j), whitePawnMoves (i, j) board ++ blackPawnMoves (i, j) board ++ knightMoves (i, j) board) | i <- [0..11], j <- [0..9], board ! (i, j) /= Empty]

printBoard :: Board -> IO ()
printBoard board = putStrLn $ unlines [rowString i | i <- [0..11]]
  where
    rowString i = concat [showSquare (board ! (i, j)) | j <- [0..9]]

    showSquare :: Piece -> String
    showSquare Empty = " . "
    showSquare WhitePawn = " P "
    showSquare BlackPawn = " p "
    showSquare Knight = " N "
    showSquare Bishop = " B "
    showSquare Rook = " R "
    showSquare Queen = " Q "
    showSquare King = " K "

-- Convert a pair of coordinates to algebraic notation
coordToAlgebraic :: (Int, Int) -> String
coordToAlgebraic (rank, file) = [toEnum (file - 1 + fromEnum 'a'), toEnum (rank - 2 + fromEnum '1')]

-- Print moves in a more human-readable format
printMoves :: [((Int, Int), [(Int, Int)])] -> IO ()
printMoves moves =
  mapM_ printMove moves
  where
    printMove :: ((Int, Int), [(Int, Int)]) -> IO ()
    printMove ((fromRank, fromFile), toSquares) = do
      let fromSquare = coordToAlgebraic (fromRank, fromFile)
      let toSquaresAlgebraic = map coordToAlgebraic toSquares
      putStrLn $ "Moves from " ++ fromSquare ++ ": " ++ show toSquaresAlgebraic

main :: IO ()
main = do 
  let board = initialBoard
  let moves = allMoves board
  printBoard board
  printMoves moves