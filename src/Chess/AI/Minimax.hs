module Chess.AI.Minimax
  ( findBestMove
  , evaluateBoard
  , evaluatePiece
  ) where

import Chess.Board
import Chess.Pieces
import Chess.Move
import Chess.Game
import Data.List (sortOn)
import Data.Ord (Down(..))
import System.Random (StdGen, randomR)

-- Minimax with Alpha-Beta pruning
findBestMove :: Int -> GameState -> StdGen -> Maybe Move
findBestMove depth gameState gen
  | null moves = Nothing 
  | otherwise = Just $ fst $ bestMoves !! (randomIndex `mod` length bestMoves)
  where
    moves = getLegalMoves gameState
    color = currentPlayer gameState
    board = gameBoard gameState
    
    sortedMoves = 
      if isCheck board color 
      then sortCheckEvasionMoves moves  -- Prioritize getting out of check
      else sortMoves moves
    
    -- Generate a random index for move selection among equally rated best moves
    (randomIndex, _) = randomR (0, max 0 (length sortedMoves - 1)) gen
    
    -- Evaluate each move using minimax
    moveScores = [(move, scoreMove move) | move <- sortedMoves]
    
    scoreMove move = 
      let newState = makeGameMove gameState move
          score = -alphaBeta (depth - 1) newState (-beta) (-alpha) (opponent color)
      in score
    
    alpha = -maxBound :: Int
    beta = maxBound :: Int
    
    -- Find best moves with maximum score
    bestScore = if null moveScores then 0 else maximum (map snd moveScores)
    bestMoves = filter ((== bestScore) . snd) moveScores

-- Basic move ordering to improve alpha-beta pruning
sortMoves :: [Move] -> [Move]
sortMoves = sortOn (Down . moveValue)
  where
    moveValue move = 
      case moveType move of
        -- Prioritize captures by the value of the captured piece minus 1/10 the value of the capturing piece
        Capture -> 
          case capturedPiece move of
            Just cap -> pieceValue (pieceType cap) - pieceValue (pieceType (movingPiece move)) `div` 10
            Nothing -> 0
        -- Prioritize promotions by the value of the promoted piece
        Promotion pt -> pieceValue pt - pieceValue Pawn
        _ -> 0

-- Sort moves specifically when in check and prioritize getting out of check
sortCheckEvasionMoves :: [Move] -> [Move]
sortCheckEvasionMoves = sortOn (Down . moveValue)
  where
    moveValue move =
      let 
        -- King moves are high priority when in check
        kingMoveValue = if pieceType (movingPiece move) == King then 1000 else 0
        
        -- Capturing values (if it's a capture)
        captureValue = case moveType move of
                         Capture -> 
                           case capturedPiece move of
                             Just cap -> pieceValue (pieceType cap)
                             Nothing -> 0
                         _ -> 0
      in 
        kingMoveValue + captureValue

-- Minimax algorithm with Alpha-Beta pruning
alphaBeta :: Int -> GameState -> Int -> Int -> Color -> Int
alphaBeta depth gameState alpha beta color
  | depth == 0 || isGameOver gameState = evaluateBoard (gameBoard gameState)
  | otherwise =
      let 
        moves = getLegalMoves gameState
        sortedMoves = sortMoves moves
      in 
        if null moves
        then
          evaluateBoard (gameBoard gameState)
        else if color == White
          then
            -- Maximizing player (White)
            searchMax sortedMoves alpha beta (-maxBound)
          else
            -- Minimizing player (Black)
            searchMin sortedMoves alpha beta maxBound
  where
    -- Search for maximizing player
    searchMax [] _ _ bestVal = bestVal
    searchMax (move:rest) a b bestVal =
      let 
        newState = makeGameMove gameState move
        childValue = alphaBeta (depth - 1) newState a b (opponent color)
        newBestVal = max bestVal childValue
        newAlpha = max a newBestVal
      in 
        if b <= newAlpha
        then newBestVal
        else searchMax rest newAlpha b newBestVal

    -- Search for minimizing player
    searchMin [] _ _ bestVal = bestVal
    searchMin (move:rest) a b bestVal =
      let 
        newState = makeGameMove gameState move
        childValue = alphaBeta (depth - 1) newState a b (opponent color)
        newBestVal = min bestVal childValue
        newBeta = min b newBestVal
      in 
        if newBeta <= a
        then newBestVal
        else searchMin rest a newBeta newBestVal

-- | Evaluate the current board position
evaluateBoard :: Board -> Int
evaluateBoard board =
  let 
    whiteMaterial = sum [evaluatePiece piece pos | pos <- allPositions, 
                         Just piece <- [getPiece board pos], 
                         pieceColor piece == White]
    blackMaterial = sum [evaluatePiece piece pos | pos <- allPositions, 
                         Just piece <- [getPiece board pos], 
                         pieceColor piece == Black]
    
    -- Mobility bonus
    whiteMoves = length (generateLegalMoves board White)
    blackMoves = length (generateLegalMoves board Black)
    mobilityBonus = (whiteMoves - blackMoves) * 5
    
    -- Check status
    whiteInCheck = isCheck board White
    blackInCheck = isCheck board Black
    checkBonus = (if blackInCheck then 50 else 0) - (if whiteInCheck then 50 else 0)
    
    -- Central control
    centralSquares = [(f, r) | f <- [2..5], r <- [2..5]]
    whiteCentral = length [(f, r) | (f, r) <- centralSquares, 
                          Just piece <- [getPiece board (f, r)], 
                          pieceColor piece == White]
    blackCentral = length [(f, r) | (f, r) <- centralSquares, 
                          Just piece <- [getPiece board (f, r)], 
                          pieceColor piece == Black]
    centralBonus = (whiteCentral - blackCentral) * 10
    
    -- Final evaluation
    eval = (whiteMaterial - blackMaterial) + mobilityBonus + checkBonus + centralBonus
  in 
    eval

-- Evaluate a piece at a specific position
evaluatePiece :: Piece -> Position -> Int
evaluatePiece piece (f, r) =
  let 
    materialValue = pieceValue (pieceType piece)
    
    -- Position-dependent bonus
    positionBonus = 
      case pieceType piece of
        Pawn -> pawnPositionValue (f, r) (pieceColor piece)
        Knight -> knightPositionValue (f, r)
        Bishop -> bishopPositionValue (f, r)
        Rook -> rookPositionValue (f, r)
        Queen -> 0
        King -> kingPositionValue (f, r) (pieceColor piece)
    
    -- positive/negative sign based on piece color
    signedValue = 
      if pieceColor piece == White
      then materialValue + positionBonus
      else negate (materialValue + positionBonus)
  in 
    signedValue

-- Position-dependent evaluation functions for each piece type
pawnPositionValue :: Position -> Color -> Int
pawnPositionValue (_, r) color =
  -- Pawns get more valuable as they advance
  case color of
    White -> r * 5
    Black -> (7 - r) * 5

knightPositionValue :: Position -> Int
knightPositionValue (f, r) =
  -- Knights are better in the center, worse in corners
  let centerDistance = abs (f - 3) + abs (r - 3)
  in 20 - (5 * centerDistance)

bishopPositionValue :: Position -> Int
bishopPositionValue _ = 0

rookPositionValue :: Position -> Int
rookPositionValue _ = 0

kingPositionValue :: Position -> Color -> Int
kingPositionValue (f, r) color =
  let 
    -- Penalty for being in the center which is unsafe
    centerPenalty = 
      if f >= 2 && f <= 5 && r >= 2 && r <= 5 
      then -30
      else 0
      
    -- King prefers to stay near its starting position
    homeBankBonus =
      case color of
        White -> if r <= 1 then 10 else 0
        Black -> if r >= 6 then 10 else 0
  in
    centerPenalty + homeBankBonus