module Chess.Move
  ( Move(..)
  , MoveType(..)
  , makeMove
  , generateMoves
  , generateLegalMoves
  , isLegalMove
  , moveToString
  , isCheck
  , isCheckmate
  , isStalemate
  , findKing
  , doesNotLeaveKingInCheck
  , generatePieceMoves
  ) where

import Chess.Board
import Chess.Pieces
import Data.Maybe (isJust, isNothing, listToMaybe, fromMaybe)

data MoveType = Normal
              | Capture
              | EnPassant
              | Castle Bool
              | Promotion PieceType
              deriving (Eq, Show)

-- Represents a chess move
data Move = Move
  { moveFrom     :: Position
  , moveTo       :: Position
  , moveType     :: MoveType
  , movingPiece  :: Piece
  , capturedPiece :: Maybe Piece
  } deriving (Eq, Show)

-- Apply a move to a board, returning a new board
makeMove :: Board -> Move -> Board
makeMove board move = 
  case moveType move of
    Normal -> 
      movePiece board (moveFrom move) (moveTo move)
    
    Capture -> 
      setPiece (removePiece board (moveFrom move)) (moveTo move) (movingPiece move)
    
    EnPassant -> 
      let capturePos = (fst (moveTo move), snd (moveFrom move))
          -- Remove the captured pawn and move the capturing pawn
      in movePiece (removePiece board capturePos) (moveFrom move) (moveTo move)
    
    Castle kingside -> 
      let (kf, kr) = moveFrom move
          kingDest = (kf + (if kingside then 2 else -2), kr)
          rookSrc = (if kingside then 7 else 0, kr)
          rookDest = (kf + (if kingside then 1 else -1), kr)
          
          -- First move the king, then move the rook
      in movePiece (movePiece board (kf, kr) kingDest) rookSrc rookDest
    
    Promotion promotionType ->
      let newPiece = Piece promotionType (pieceColor (movingPiece move))
      in setPiece (removePiece board (moveFrom move)) (moveTo move) newPiece

-- Check if a move is legal
isLegalMove :: Board -> Move -> Bool
isLegalMove board move = 
  move `elem` generateLegalMoves board (pieceColor (movingPiece move))

-- All possible moves for a given color
generateMoves :: Board -> Color -> [Move]
generateMoves board color = 
  concatMap (generatePieceMoves board) 
    [(pos, piece) | pos <- allPositions, 
                   Just piece <- [getPiece board pos], 
                   pieceColor piece == color]

-- Moves that don't leave the king in check
generateLegalMoves :: Board -> Color -> [Move]
generateLegalMoves board color = 
  filter (doesNotLeaveKingInCheck board) (generateMoves board color)

doesNotLeaveKingInCheck :: Board -> Move -> Bool
doesNotLeaveKingInCheck board move =
  let newBoard = makeMove board move
  in not (isCheck newBoard (pieceColor (movingPiece move)))

-- All possible moves for a specific piece
generatePieceMoves :: Board -> (Position, Piece) -> [Move]
generatePieceMoves board (pos, piece) =
  case pieceType piece of
    Pawn   -> generatePawnMoves board pos piece
    Knight -> generateKnightMoves board pos piece
    Bishop -> generateSlidingMoves board pos piece bishopDirections
    Rook   -> generateSlidingMoves board pos piece rookDirections
    Queen  -> generateSlidingMoves board pos piece (bishopDirections ++ rookDirections)
    King   -> generateKingMoves board pos piece

-- Sliding pieces
rookDirections :: [Position]
rookDirections = [(0,1), (1,0), (0,-1), (-1,0)]

bishopDirections :: [Position]
bishopDirections = [(1,1), (1,-1), (-1,-1), (-1,1)]

-- Pawn moves
generatePawnMoves :: Board -> Position -> Piece -> [Move]
generatePawnMoves board (f, r) piece = 
  let color = pieceColor piece
      direction = if color == White then 1 else -1
      startRank = if color == White then 1 else 6
      
      oneStep = (f, r + direction)
      twoStep = (f, r + 2 * direction)
      
      -- If a rank is a promotion rank
      isPromotionRank (_, rank) = rank == (if color == White then 7 else 0)
      promotionPieces = [Queen, Rook, Bishop, Knight]
      
      -- Basic move
      createMove from to moveTypeVal =
        Move from to moveTypeVal piece (getPiece board to)
      
      -- One step forward
      forwardMoves = 
        if isValidPosition oneStep && isNothing (getPiece board oneStep)
        then if isPromotionRank oneStep
             then [createMove (f, r) oneStep (Promotion pt) | pt <- promotionPieces]
             else [createMove (f, r) oneStep Normal]
        else []
      
      -- Two steps forward
      twoStepMoves =
        if r == startRank && 
           isValidPosition oneStep && isNothing (getPiece board oneStep) &&
           isValidPosition twoStep && isNothing (getPiece board twoStep)
        then [createMove (f, r) twoStep Normal]
        else []
      
      -- Capture moves
      capturePositions = [(f + df, r + direction) | df <- [-1, 1]]
      
      captureMoves = 
        [createMove (f, r) capturePos (if isPromotionRank capturePos then Promotion pt else Capture) | 
         capturePos <- capturePositions, 
         isValidPosition capturePos, 
         Just targetPiece <- [getPiece board capturePos], 
         pieceColor targetPiece /= color,
         pt <- if isPromotionRank capturePos then promotionPieces else [undefined]]
         
      -- En passant captures
      enPassantCaptures =
        [Move (f, r) (f + df, r + direction) EnPassant piece (Just (Piece Pawn (opponent color))) | 
         df <- [-1, 1], 
         isValidPosition (f + df, r),
         isValidPosition (f + df, r + direction),
         Just (Piece Pawn enemyColor) <- [getPiece board (f + df, r)],
         enemyColor /= color,
         isEnPassantPosition (f + df, r)]
         
      isEnPassantPosition _ = False 
      
  in forwardMoves ++ twoStepMoves ++ captureMoves ++ enPassantCaptures

-- All possible knight moves
generateKnightMoves :: Board -> Position -> Piece -> [Move]
generateKnightMoves board (f, r) piece =
  let color = pieceColor piece
      knightOffsets = [(1,2), (2,1), (2,-1), (1,-2), (-1,-2), (-2,-1), (-2,1), (-1,2)]
      destinations = [(f + df, r + dr) | (df, dr) <- knightOffsets, isValidPosition (f + df, r + dr)]
      
      validMoves = 
        [Move (f, r) dest (if isJust (getPiece board dest) then Capture else Normal) piece (getPiece board dest) | 
         dest <- destinations,
         case getPiece board dest of
           Nothing -> True
           Just targetPiece -> pieceColor targetPiece /= color
        ]
  in validMoves

-- Sliding moves (bishop, rook, queen)
generateSlidingMoves :: Board -> Position -> Piece -> [Position] -> [Move]
generateSlidingMoves board (f, r) piece directions =
  let color = pieceColor piece
      
      movesInDirection (df, dr) = 
        takeWhileInclusive 
          (\pos -> isValidPosition pos && 
                   (isNothing (getPiece board pos) || 
                    pieceColor (fromMaybe undefined (getPiece board pos)) /= color))
          [(f + i*df, r + i*dr) | i <- [1..7], isValidPosition (f + i*df, r + i*dr)]
      
      -- Take positions until the first blocking piece
      takeWhileInclusive _ [] = []
      takeWhileInclusive p (x:xs) = 
        if p x then x : (if isJust (getPiece board x) then [] else takeWhileInclusive p xs)
        else []
      
      destinations = concatMap movesInDirection directions
      
      validMoves = 
        [Move (f, r) dest (if isJust (getPiece board dest) then Capture else Normal) piece (getPiece board dest) | 
         dest <- destinations]
  in validMoves

-- Generate all possible king moves including castling
generateKingMoves :: Board -> Position -> Piece -> [Move]
generateKingMoves board (f, r) piece =
  let color = pieceColor piece
      kingOffsets = [(0,1), (1,1), (1,0), (1,-1), (0,-1), (-1,-1), (-1,0), (-1,1)]
      destinations = [(f + df, r + dr) | (df, dr) <- kingOffsets, isValidPosition (f + df, r + dr)]
      
      standardMoves = 
        [Move (f, r) dest (if isJust (getPiece board dest) then Capture else Normal) piece (getPiece board dest) | 
         dest <- destinations,
         case getPiece board dest of
           Nothing -> True
           Just targetPiece -> pieceColor targetPiece /= color
        ]
        
      castlingMoves = []
      
  in standardMoves ++ castlingMoves

-- Move to algebraic notation
moveToString :: Move -> String
moveToString move =
  case moveType move of
    Castle True -> "O-O"
    Castle False -> "O-O-O"
    _ -> 
      let from = positionToString (moveFrom move)
          to = positionToString (moveTo move)
          piece = case pieceType (movingPiece move) of
                    Pawn -> ""
                    Knight -> "N"
                    Bishop -> "B"
                    Rook -> "R"
                    Queen -> "Q"
                    King -> "K"
          capture = if moveType move `elem` [Capture, EnPassant] then "x" else ""
          promotion = case moveType move of
                        Promotion pt -> "=" ++ case pt of
                                               Knight -> "N"
                                               Bishop -> "B"
                                               Rook -> "R"
                                               Queen -> "Q"
                                               _ -> ""
                        _ -> ""
      in piece ++ from ++ capture ++ to ++ promotion

-- King's position for a given color
findKing :: Board -> Color -> Maybe Position
findKing board color = 
  listToMaybe [pos | pos <- allPositions, 
               Just (Piece King c) <- [getPiece board pos], 
               c == color]

-- Check if a king is in check
isCheck :: Board -> Color -> Bool
isCheck board color =
  case findKing board color of
    Nothing -> True  -- If king is missing, consider it in "check"
    Just kingPos ->
      any (canAttack board kingPos) (opponentPieces board color)

-- Get all opponent pieces with their positions
opponentPieces :: Board -> Color -> [(Position, Piece)]
opponentPieces board color =
  [(pos, piece) | pos <- allPositions, 
                 Just piece <- [getPiece board pos], 
                 pieceColor piece /= color]

canAttack :: Board -> Position -> (Position, Piece) -> Bool
canAttack board targetPos (piecePos, piece) =
  any (\move -> moveTo move == targetPos) (generatePieceMoves board (piecePos, piece))

isCheckmate :: Board -> Color -> Bool
isCheckmate board color =
  isCheck board color && null (generateLegalMoves board color)

isStalemate :: Board -> Color -> Bool
isStalemate board color =
  not (isCheck board color) && null (generateLegalMoves board color)