module Chess.Board
  (
    Board
  , Position
  , File
  , Rank
  , initialBoard
  , emptyBoard
  , getPiece
  , setPiece
  , removePiece
  , movePiece
  , isValidPosition
  , positionToString
  , stringToPosition
  , allPositions
  , fileToChar
  , rankToChar
  , charToFile
  , charToRank
  ) where

import qualified Data.Map.Strict as Map
import Chess.Pieces

-- column, row and position on the chess board
type File = Int
type Rank = Int
type Position = (File, Rank)

-- Chess board represented as a map from positions to pieces
newtype Board = Board (Map.Map Position Piece)
  deriving (Eq, Show)

emptyBoard :: Board
emptyBoard = Board Map.empty

-- Initial chess board position
initialBoard :: Board
initialBoard = Board $ Map.fromList (backRank ++ pawns)
  where
    backRankPieces = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
    
    backRank = 
      concatMap (\(c, r) -> zipWith (\f p -> ((f, r), Piece p c)) [0..7] backRankPieces)
                [(White, 0), (Black, 7)]
    
    pawns = 
      concatMap (\(c, r) -> [((f, r), Piece Pawn c) | f <- [0..7]]) 
                [(White, 1), (Black, 6)]

isValidPosition :: Position -> Bool
isValidPosition (f, r) = f >= 0 && f <= 7 && r >= 0 && r <= 7

-- Get a piece at a given position
getPiece :: Board -> Position -> Maybe Piece
getPiece (Board board) pos 
  | isValidPosition pos = Map.lookup pos board
  | otherwise = Nothing

-- Set a piece at a given position
setPiece :: Board -> Position -> Piece -> Board
setPiece (Board board) pos piece
  | isValidPosition pos = Board $ Map.insert pos piece board
  | otherwise = Board board

-- Remove a piece from a given position
removePiece :: Board -> Position -> Board
removePiece (Board board) pos
  | isValidPosition pos = Board $ Map.delete pos board
  | otherwise = Board board 

-- Move a piece from one position to another
movePiece :: Board -> Position -> Position -> Board
movePiece board from to
  | not (isValidPosition from) || not (isValidPosition to) = board
  | otherwise = case getPiece board from of
      Nothing -> board
      Just piece -> setPiece (removePiece board from) to piece

-- Position to algebraic notation
positionToString :: Position -> String
positionToString (f, r) = [fileToChar f, rankToChar r]

-- Algebraic notation to a position
stringToPosition :: String -> Maybe Position
stringToPosition [f, r] = do
  file <- charToFile f
  rank <- charToRank r
  return (file, rank)
stringToPosition _ = Nothing

-- All valid positions on the board
allPositions :: [Position]
allPositions = [(f, r) | f <- [0..7], r <- [0..7]]

-- File index to its character representation
fileToChar :: File -> Char
fileToChar f = toEnum (fromEnum 'a' + f)

-- Rank index to its character representation
rankToChar :: Rank -> Char
rankToChar r = toEnum (fromEnum '1' + r)

-- Character to a file index
charToFile :: Char -> Maybe File
charToFile c 
  | c >= 'a' && c <= 'h' = Just $ fromEnum c - fromEnum 'a'
  | otherwise = Nothing

-- Character to a rank index
charToRank :: Char -> Maybe Rank
charToRank c
  | c >= '1' && c <= '8' = Just $ fromEnum c - fromEnum '1'
  | otherwise = Nothing