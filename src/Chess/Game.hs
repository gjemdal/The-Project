module Chess.Game
  ( GameState(..)
  , GameResult(..)
  , CastlingRights(..)
  , initialGameState
  , makeGameMove
  , getGameResult
  , isGameOver
  , getCurrentPlayer
  , getLegalMoves
  , canCastle
  ) where

import Chess.Board
import Chess.Pieces
import Chess.Move
import Data.Maybe (isJust, isNothing)

data CastlingRights = CastlingRights
  { whiteKingside  :: Bool
  , whiteQueenside :: Bool
  , blackKingside  :: Bool
  , blackQueenside :: Bool
  } deriving (Eq, Show)

data GameResult = WhiteWin
                | BlackWin
                | Draw
                | InProgress
                deriving (Eq, Show)

-- Complete state of a chess game
data GameState = GameState
  { gameBoard         :: Board
  , currentPlayer     :: Color
  , castlingRights    :: CastlingRights
  , enPassantTarget   :: Maybe Position
  , halfmoveClock     :: Int
  , fullmoveNumber    :: Int
  , moveHistory       :: [Move]
  } deriving (Eq, Show)

-- Initial game state for a standard chess game
initialGameState :: GameState
initialGameState = GameState
  { gameBoard = initialBoard
  , currentPlayer = White
  , castlingRights = CastlingRights True True True True
  , enPassantTarget = Nothing
  , halfmoveClock = 0
  , fullmoveNumber = 1
  , moveHistory = []
  }

makeGameMove :: GameState -> Move -> GameState
makeGameMove gs move =
  -- Only apply the move if it's legal and it's the moving piece's turn
  if pieceColor (movingPiece move) /= currentPlayer gs ||
     not (move `elem` getLegalMoves gs)
  then gs  -- Illegal move, no change
  else GameState
    { gameBoard = makeMove (gameBoard gs) move
    , currentPlayer = opponent (currentPlayer gs)
    , castlingRights = updateCastlingRights gs move
    , enPassantTarget = calculateEnPassantTarget move
    , halfmoveClock = updateHalfmoveClock gs move
    , fullmoveNumber = updateFullmoveNumber gs
    , moveHistory = move : moveHistory gs
    }

-- Update castling rights based on a move
updateCastlingRights :: GameState -> Move -> CastlingRights
updateCastlingRights gs move =
  let rights = castlingRights gs
      pt = pieceType (movingPiece move)
      color = pieceColor (movingPiece move)
      from = moveFrom move
      
      newRights = 
        case (color, pt) of
          (White, King) -> rights { whiteKingside = False, whiteQueenside = False }
          (Black, King) -> rights { blackKingside = False, blackQueenside = False }
          (White, Rook) -> case from of
                             (0, 0) -> rights { whiteQueenside = False }
                             (7, 0) -> rights { whiteKingside = False }
                             _      -> rights
          (Black, Rook) -> case from of
                             (0, 7) -> rights { blackQueenside = False }
                             (7, 7) -> rights { blackKingside = False }
                             _      -> rights
          _             -> rights
  in newRights

-- Calculate en passant target if any
calculateEnPassantTarget :: Move -> Maybe Position
calculateEnPassantTarget move =
  let (f, r) = moveFrom move
      (_, toR) = moveTo move
      piece = movingPiece move
  in 
    if pieceType piece == Pawn && abs (toR - r) == 2
    then 
      let direction = if pieceColor piece == White then 1 else -1
      in Just (f, r + direction)
    else Nothing

-- Halfmove clock for the 50-move rule which resets on pawn move or capture, otherwise increment
updateHalfmoveClock :: GameState -> Move -> Int
updateHalfmoveClock gs move =
  if pieceType (movingPiece move) == Pawn || isJust (capturedPiece move)
  then 0
  else halfmoveClock gs + 1

updateFullmoveNumber :: GameState -> Int
updateFullmoveNumber gs =
  if currentPlayer gs == Black
  then fullmoveNumber gs + 1
  else fullmoveNumber gs

-- Check if the game is over and determine result
getGameResult :: GameState -> GameResult
getGameResult gs =
  let board = gameBoard gs
      player = currentPlayer gs
      inCheck = isCheck board player
      noLegalMoves = null (getLegalMoves gs)
      insufficientMaterial = checkInsufficientMaterial board
      fiftyMoveRule = halfmoveClock gs >= 100
  in 
    if inCheck && noLegalMoves && player == White
    then BlackWin
    else if inCheck && noLegalMoves && player == Black
    then WhiteWin
    else if (not inCheck && noLegalMoves) || insufficientMaterial || fiftyMoveRule
    then Draw
    else InProgress

-- | Check if there's insufficient material for checkmate
checkInsufficientMaterial :: Board -> Bool
checkInsufficientMaterial board =
  let pieces = [(pos, piece) | pos <- allPositions, 
                Just piece <- [getPiece board pos]]
      
      -- Count pieces by type and color
      whitePieces = [p | (_, p) <- pieces, pieceColor p == White]
      blackPieces = [p | (_, p) <- pieces, pieceColor p == Black]
      
      -- Common insufficient material scenarios
      kingsOnly = length whitePieces == 1 && length blackPieces == 1 && 
                 all (\p -> pieceType p == King) whitePieces && 
                 all (\p -> pieceType p == King) blackPieces
                 
      kingAndMinorPiece color otherColor = 
        length (filter (\p -> pieceType p == King) color) == 1 &&
        length color == 2 && 
        any (\p -> pieceType p `elem` [Bishop, Knight]) color &&
        length otherColor == 1 && 
        all (\p -> pieceType p == King) otherColor
  in 
    kingsOnly || 
    kingAndMinorPiece whitePieces blackPieces || 
    kingAndMinorPiece blackPieces whitePieces

isGameOver :: GameState -> Bool
isGameOver gs = getGameResult gs /= InProgress

getCurrentPlayer :: GameState -> Color
getCurrentPlayer = currentPlayer

-- All legal moves for the current player
getLegalMoves :: GameState -> [Move]
getLegalMoves gs = 
  let board = gameBoard gs
      color = currentPlayer gs
      baseMoves = generateLegalMoves board color
      castleMoves = generateCastlingMoves gs
      enPassantMoves = generateEnPassantMoves gs
      
  in baseMoves ++ castleMoves ++ enPassantMoves

-- En passant moves if available
generateEnPassantMoves :: GameState -> [Move]
generateEnPassantMoves gs =
  case enPassantTarget gs of
    Nothing -> []
    Just target -> 
      let board = gameBoard gs
          color = currentPlayer gs
          targetFile = fst target
          pawnRank = if color == White then 4 else 3
          
          -- Check for pawns that could capture en passant
          possiblePawns = [(file, pawnRank) | file <- [targetFile-1, targetFile+1], 
                           file >= 0 && file <= 7,
                           Just (Piece Pawn c) <- [getPiece board (file, pawnRank)],
                           c == color]
          
          -- Create en passant moves
          moves = [Move from target EnPassant (Piece Pawn color) (Just (Piece Pawn (opponent color))) 
                  | from <- possiblePawns]
          
          -- Filter out moves that would leave the king in check
          legalMoves = filter (doesNotLeaveKingInCheck board) moves
      in legalMoves

-- Castling moves if available
generateCastlingMoves :: GameState -> [Move]
generateCastlingMoves gs =
  let board = gameBoard gs
      color = currentPlayer gs
      
      kingPos = case findKing board color of
                  Nothing -> (-1, -1)
                  Just pos -> pos
      
      canCastleKingside = canCastle gs color True
      canCastleQueenside = canCastle gs color False
      kingsideCastle = 
        if canCastleKingside
        then [Move kingPos (fst kingPos + 2, snd kingPos) (Castle True) (Piece King color) Nothing]
        else []
        
      queensideCastle = 
        if canCastleQueenside
        then [Move kingPos (fst kingPos - 2, snd kingPos) (Castle False) (Piece King color) Nothing]
        else []
  
  in kingsideCastle ++ queensideCastle

-- Check if castling is possible
canCastle :: GameState -> Color -> Bool -> Bool
canCastle gs color kingside =
  let board = gameBoard gs
      crights = castlingRights gs
      
      hasRight = case (color, kingside) of
                   (White, True) -> whiteKingside crights
                   (White, False) -> whiteQueenside crights
                   (Black, True) -> blackKingside crights
                   (Black, False) -> blackQueenside crights
      
      kingRank = if color == White then 0 else 7
      kingFile = 4
      kingPos = (kingFile, kingRank)
      correctKingPos = case getPiece board kingPos of
                         Just (Piece King c) | c == color -> True
                         _ -> False
      
      -- Check if path is clear
      pathClear = 
        if kingside
        then all (\f -> isNothing (getPiece board (f, kingRank))) [5, 6]
        else all (\f -> isNothing (getPiece board (f, kingRank))) [1, 2, 3]
      
      -- Check if rook is present
      rookPos = (if kingside then 7 else 0, kingRank)
      rookPresent = case getPiece board rookPos of
                      Just (Piece Rook c) | c == color -> True
                      _ -> False
      
      -- Check if squares are under attack
      squaresUnderAttack = 
        if kingside
        then any (isSquareAttacked board color) [(4, kingRank), (5, kingRank), (6, kingRank)]
        else any (isSquareAttacked board color) [(2, kingRank), (3, kingRank), (4, kingRank)]
  
  in hasRight && correctKingPos && pathClear && rookPresent && not squaresUnderAttack

-- Check if a square is attacked by an opponent
isSquareAttacked :: Board -> Color -> Position -> Bool
isSquareAttacked board color pos =
  let enemyColor = opponent color
      enemyPieces = [(p, piece) | p <- allPositions, 
                     Just piece <- [getPiece board p], 
                     pieceColor piece == enemyColor]
  in any (\piecePos -> any (\move -> moveTo move == pos) (generatePieceMoves board piecePos)) enemyPieces