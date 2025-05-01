module Chess.FEN
  ( gameToFEN
  , fenToGame
  , startingPositionFEN
  ) where

import Chess.Board
import Chess.Pieces
import Chess.Game
import Data.Char (isDigit, digitToInt)
import Data.List (intercalate)

-- Standard starting position in FEN notation
startingPositionFEN :: String
startingPositionFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

gameToFEN :: GameState -> String
gameToFEN gs =
  let 
    board = gameBoard gs
    
    -- 1. Piece placement (board representation)
    ranks = reverse [0..7]  -- FEN goes from rank 8 to 1
    boardString = intercalate "/" [rankToFEN board r | r <- ranks]
    
    -- 2. Active color
    activeColor = if currentPlayer gs == White then "w" else "b"
    
    -- 3. Castling availability
    rights = castlingRights gs
    castling = [
        if whiteKingside rights then "K" else "",
        if whiteQueenside rights then "Q" else "",
        if blackKingside rights then "k" else "",
        if blackQueenside rights then "q" else ""
      ]
    castlingStr = if null (concat castling) then "-" else concat castling
    
    -- 4. En passant target square
    enPassant = maybe "-" positionToString (enPassantTarget gs)
    
    -- 5 & 6. Halfmove clock and fullmove number
    halfmove = show (halfmoveClock gs)
    fullmove = show (fullmoveNumber gs)
    
  in intercalate " " [boardString, activeColor, castlingStr, enPassant, halfmove, fullmove]

-- | Convert a rank to FEN notation
rankToFEN :: Board -> Rank -> String
rankToFEN board rank =
  let 
    -- Get piece at each position in the rank, or empty space
    rankPieces = [getPiece board (file, rank) | file <- [0..7]]
    
    -- Convert the list of pieces to FEN, compressing empty squares
    compress [] = []
    compress (Nothing:rest) = 
      let (empties, remainder) = span (== Nothing) rest
          emptyCount = 1 + length empties
      in show emptyCount ++ compress remainder
    compress (Just p:rest) = pieceToChar p : compress rest
  in
    compress rankPieces

-- Convert a FEN string to GameState
fenToGame :: String -> Maybe GameState
fenToGame fen = 
  case words fen of
    [boardStr, activeStr, castlingStr, enPassantStr, halfmoveStr, fullmoveStr] -> do
      board <- parseFENBoard boardStr
      active <- parseFENActive activeStr
      castling <- parseFENCastling castlingStr
      enPassant <- parseFENEnPassant enPassantStr
      halfmove <- readMaybe halfmoveStr
      fullmove <- readMaybe fullmoveStr
      
      return GameState
        { gameBoard = board
        , currentPlayer = active
        , castlingRights = castling
        , enPassantTarget = enPassant
        , halfmoveClock = halfmove
        , fullmoveNumber = fullmove
        , moveHistory = []
        }
    _ -> Nothing

-- Parse the board position part of FEN
parseFENBoard :: String -> Maybe Board
parseFENBoard str = 
  let ranks = split '/' str
  in if length ranks /= 8
     then Nothing
     else Just $ parseFENRanks emptyBoard $ zip [7,6..0] ranks

-- Parse ranks from FEN notation and place pieces on the board
parseFENRanks :: Board -> [(Rank, String)] -> Board
parseFENRanks board [] = board
parseFENRanks board ((rank, rankStr):rest) =
  let 
    parseRank :: Board -> Int -> String -> Board
    parseRank b _ [] = b
    parseRank b file (c:cs)
      | isDigit c = parseRank b (file + digitToInt c) cs
      | otherwise = case charToPiece c of
                      Just piece -> parseRank (setPiece b (file, rank) piece) (file + 1) cs
                      Nothing -> parseRank b (file + 1) cs
  in
    parseFENRanks (parseRank board 0 rankStr) rest

-- Parse the active color part of FEN
parseFENActive :: String -> Maybe Color
parseFENActive "w" = Just White
parseFENActive "b" = Just Black
parseFENActive _ = Nothing

-- Parse the castling availability part of FEN
parseFENCastling :: String -> Maybe CastlingRights
parseFENCastling str
  | str == "-" = Just $ CastlingRights False False False False
  | otherwise = Just $ CastlingRights 
                  ('K' `elem` str) 
                  ('Q' `elem` str) 
                  ('k' `elem` str) 
                  ('q' `elem` str)

-- Parse the en passant target square part of FEN
parseFENEnPassant :: String -> Maybe (Maybe Position)
parseFENEnPassant "-" = Just Nothing
parseFENEnPassant str = Just (stringToPosition str)

-- Split a string by a delimiter
split :: Char -> String -> [String]
split _ "" = []
split c s = 
  let (before, rest) = break (== c) s
  in before : case rest of
                [] -> []
                (_:rs) -> split c rs

-- Read a Maybe value
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                [(x, "")] -> Just x
                _ -> Nothing