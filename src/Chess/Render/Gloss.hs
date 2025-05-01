module Chess.Render.Gloss
  ( runGlossGame
  , runGlossAI
  ) where

import Chess.Board
import Chess.Pieces (Piece(..), PieceType(..))
import qualified Chess.Pieces as CP
import Chess.Move (Move(..), MoveType(..), moveToString, isCheck)
import Chess.Game
import Chess.AI.Minimax (findBestMove)
import Chess.FEN (gameToFEN, fenToGame)
import Graphics.Gloss hiding (Color)
import qualified Graphics.Gloss as G
import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), KeyState(..), MouseButton(..))
import Graphics.Gloss.Interface.IO.Game (playIO)
import Graphics.Gloss.Juicy (loadJuicyPNG)
import System.Random (StdGen, newStdGen, mkStdGen)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Control.Exception (try, SomeException)
import qualified Data.Map as Map
import Control.Monad (foldM)
import Data.Maybe (fromMaybe)

-- Board layout
squareSize, boardSize, offset :: Float
squareSize = 60
boardSize = 8 * squareSize
offset = boardSize / 2

data GlossGame = GlossGame
  { glossGameState :: GameState
  , glossSelected  :: Maybe Position
  , glossAIPlayer  :: Maybe CP.Color
  , glossAIDepth   :: Int
  , glossMessage   :: String
  , glossRandom    :: StdGen
  , glossPieceImages :: Map.Map String Picture
  }

-- Two-player chess game
runGlossGame :: IO ()
runGlossGame = do
  pieceImages <- loadPieceImages
  let initialGlossState = GlossGame
        { glossGameState = initialGameState
        , glossSelected = Nothing
        , glossAIPlayer = Nothing
        , glossAIDepth = 0
        , glossMessage = "White to move. Press 's' to save, 'l' to load."
        , glossRandom = mkStdGen 42
        , glossPieceImages = pieceImages
        }
  
  playIO
    (InWindow "Chess Game" (round boardSize + 200, round boardSize + 100) (10, 10))
    (makeColor 0.2 0.2 0.2 1.0)
    30
    initialGlossState
    drawGameIO
    handleEventIO
    (\_ state -> return state)

-- Game against the AI
runGlossAI :: CP.Color -> Int -> IO ()
runGlossAI aiColor depth = do
  pieceImages <- loadPieceImages
  stdGen <- newStdGen
  let initialGlossState = GlossGame
        { glossGameState = initialGameState
        , glossSelected = Nothing
        , glossAIPlayer = Just aiColor
        , glossAIDepth = depth
        , glossMessage = "Game started. " ++ if aiColor == CP.White then "Your move (Black)" else "AI thinking..."
        , glossRandom = stdGen
        , glossPieceImages = pieceImages
        }
      
  let startState = if aiColor == CP.White then makeAIMove initialGlossState else initialGlossState
  
  playIO
    (InWindow "Chess Game vs AI" (round boardSize + 200, round boardSize + 100) (10, 10))
    (makeColor 0.2 0.2 0.2 1.0)
    30
    startState
    drawGameIO
    handleEventIO
    (\_ state -> do
      let gs = glossGameState state
          curPlayer = currentPlayer gs
          isAIMoving = isAITurn state
          isFinished = isGameOver gs || isInCheckmatePiece (gameBoard gs) curPlayer
      
      if isFinished then return state
      else if isAIMoving then return (makeAIMove state)
      else return state)

isInCheckmatePiece :: Board -> CP.Color -> Bool
isInCheckmatePiece board playerColor = isCheck board playerColor && null (getLegalMoves (GameState board playerColor (CastlingRights False False False False) Nothing 0 0 []))

-- Save/load functions
saveGameToFEN :: GlossGame -> FilePath -> IO GlossGame
saveGameToFEN game filePath = do
  let fenString = gameToFEN (glossGameState game)
  _ <- try (writeFile filePath fenString) :: IO (Either SomeException ())
  return $ game { glossMessage = "Game saved to " ++ filePath }

loadGameFromFEN :: FilePath -> GlossGame -> IO GlossGame
loadGameFromFEN filePath game = do
  result <- try (readFile filePath) :: IO (Either SomeException String)
  case result of
    Left _ -> return $ game { glossMessage = "Failed to load game" }
    Right fenString -> 
      case fenToGame fenString of
        Nothing -> return $ game { glossMessage = "Invalid FEN format" }
        Just newState -> return $ game { glossGameState = newState
                                       , glossSelected = Nothing
                                       , glossMessage = "Game loaded from " ++ filePath }

-- Load chess piece images
loadPieceImages :: IO (Map.Map String Picture)
loadPieceImages = do
  let pieceTypes = ["pawn", "knight", "bishop", "rook", "queen", "king"]
      colorNames = ["white", "black"]
      imageNames = [colorName ++ "_" ++ pieceName | colorName <- colorNames, pieceName <- pieceTypes]
      imagePaths = map (\name -> "images" </> name ++ ".png") imageNames
      fallback = scale 0.4 0.4 $ G.color white $ text "?"
  
  foldM (\m (name, path) -> do
          exists <- doesFileExist path
          if not exists then return $ Map.insert name fallback m
          else do
            maybePic <- loadJuicyPNG path
            case maybePic of
              Just pic -> return $ Map.insert name (scale (squareSize / 400) (squareSize / 400) pic) m
              Nothing -> return $ Map.insert name fallback m
        ) Map.empty (zip imageNames imagePaths)

-- IO drawing and event handling wrappers
drawGameIO :: GlossGame -> IO Picture
drawGameIO = return . drawGame

handleEventIO :: Event -> GlossGame -> IO GlossGame
handleEventIO (EventKey (Char 's') Down _ _) game = saveGameToFEN game "chess_save.fen"
handleEventIO (EventKey (Char 'l') Down _ _) game = loadGameFromFEN "chess_save.fen" game
handleEventIO event game = return $ handleEvent event game

-- Check if AI's turn
isAITurn :: GlossGame -> Bool
isAITurn game =
  case glossAIPlayer game of
    Nothing -> False
    Just aiColor -> 
      let gs = glossGameState game
          curPlayer = currentPlayer gs
          board = gameBoard gs
      in curPlayer == aiColor && not (isGameOver gs || isInCheckmatePiece board curPlayer)

makeAIMove :: GlossGame -> GlossGame
makeAIMove game =
  case glossAIPlayer game of
    Nothing -> game
    Just aiColor ->
      if currentPlayer (glossGameState game) /= aiColor then game
      else case findBestMove (glossAIDepth game) (glossGameState game) (glossRandom game) of
             Nothing -> game { glossMessage = "AI could not find a move!" }
             Just move ->
               let newState = makeGameMove (glossGameState game) move
                   opponentColor = CP.opponent aiColor
                   board = gameBoard newState
                   checkmateDetected = isInCheckmatePiece board opponentColor
                   
                   moveDesc = case moveType move of
                     Castle True -> "castled kingside"
                     Castle False -> "castled queenside"
                     EnPassant -> "captured en passant"
                     _ -> "moved " ++ moveToString move
                   
                   newMessage = if checkmateDetected then "Checkmate! AI wins!"
                               else if isGameOver newState then "Game over: " ++ show (getGameResult newState)
                               else "Your move"
               in game { glossGameState = newState
                        , glossMessage = "AI " ++ moveDesc ++ ". " ++ newMessage
                        }

drawGame :: GlossGame -> Picture
drawGame game =
  let gs = glossGameState game
      board = gameBoard gs
      curPlayer = currentPlayer gs
      
      boardPic = drawBoard gs (glossSelected game) (glossPieceImages game)
      
      statusText = if isInCheckmatePiece board curPlayer then "CHECKMATE!"
                   else if isCheck board curPlayer then "CHECK!"
                   else if isGameOver gs then "GAME OVER: " ++ show (getGameResult gs)
                   else show curPlayer ++ " to move"
      
      messagePic = translate (-offset + 10) (-offset - 40) $
                   scale 0.15 0.15 $
                   G.color white $  
                   text (glossMessage game)
      
      statusPic = translate (-offset + 10) (offset + 20) $
                  scale 0.15 0.15 $
                  G.color (if "CHECK" `elem` words statusText then red else white) $  
                  text statusText
  in pictures [boardPic, messagePic, statusPic]

drawBoard :: GameState -> Maybe Position -> Map.Map String Picture -> Picture
drawBoard gameState selected pieceImages =
  let board = gameBoard gameState
      border = G.color (makeColor 0.7 0.5 0.3 1.0) $
               rectangleSolid (boardSize + 20) (boardSize + 20)
      
      squares = pictures [drawSquare (f, r) | f <- [0..7], r <- [0..7]]
      
      pieces = pictures [drawPiece pos piece pieceImages | pos <- allPositions,
                         Just piece <- [getPiece board pos]]
      
      highlight = case selected of
                    Nothing -> blank
                    Just pos -> 
                      let highlightSquare = drawHighlight pos
                          validMoves = case getPiece board pos of
                                        Just piece | CP.pieceColor piece == currentPlayer gameState ->
                                          [move | move <- getLegalMoves gameState, moveFrom move == pos]
                                        _ -> []
                          highlightMoves = pictures [drawMoveHighlight (moveTo move) (getMoveHighlightColor move) | move <- validMoves]
                      in pictures [highlightSquare, highlightMoves]
      
      -- Coordinate labels
      labels = pictures [drawFileLabel f | f <- [0..7]] <> pictures [drawRankLabel r | r <- [0..7]]
      
  in pictures [border, squares, highlight, pieces, labels]

-- Get different highlight color based on move type
getMoveHighlightColor :: Move -> G.Color
getMoveHighlightColor move = 
  case moveType move of
    Normal -> makeColor 0.0 0.0 1.0 0.3
    Capture -> makeColor 1.0 0.0 0.0 0.3
    EnPassant -> makeColor 1.0 0.0 1.0 0.3
    Castle _ -> makeColor 0.0 1.0 1.0 0.3
    Promotion _ -> makeColor 0.0 1.0 0.0 0.3

-- Draw a square
drawSquare :: Position -> Picture
drawSquare (f, r) =
  translate (fromIntegral f * squareSize - offset + squareSize/2)
            (fromIntegral r * squareSize - offset + squareSize/2) $
  G.color (squareColor (f, r)) $
  rectangleSolid squareSize squareSize
  where
    squareColor (f', r') = 
      if even (f' + r')
      then makeColor 0.93 0.93 0.82 1.0
      else makeColor 0.46 0.59 0.34 1.0

-- Draw a chess piece
drawPiece :: Position -> Piece -> Map.Map String Picture -> Picture
drawPiece (f, r) piece pieceImages =
  translate (fromIntegral f * squareSize - offset + squareSize/2)
            (fromIntegral r * squareSize - offset + squareSize/2) $
  fromMaybe fallbackPic (Map.lookup (pieceImageName piece) pieceImages)
  where
    fallbackPic = scale 0.4 0.4 $ 
                 G.color (if CP.pieceColor piece == CP.White then white else black) $
                 text [pieceTypeChar (CP.pieceType piece)]
    
    pieceTypeChar Pawn = 'P'
    pieceTypeChar Knight = 'N'
    pieceTypeChar Bishop = 'B'
    pieceTypeChar Rook = 'R'
    pieceTypeChar Queen = 'Q'
    pieceTypeChar King = 'K'
    
    pieceImageName p =
      let colorName = if CP.pieceColor p == CP.White then "white" else "black"
          pieceName = case CP.pieceType p of
                        Pawn -> "pawn"
                        Knight -> "knight"
                        Bishop -> "bishop"
                        Rook -> "rook"
                        Queen -> "queen"
                        King -> "king"
      in colorName ++ "_" ++ pieceName

-- Draw coordinate labels and highlights
drawFileLabel :: Int -> Picture
drawFileLabel f =
  translate (fromIntegral f * squareSize - offset + squareSize/2) (-offset - 15) $
  scale 0.15 0.15 $ G.color white $ text [fileToChar f]

drawRankLabel :: Int -> Picture
drawRankLabel r =
  translate (-offset - 15) (fromIntegral r * squareSize - offset + squareSize/2) $
  scale 0.15 0.15 $ G.color white $ text [rankToChar r]

drawHighlight :: Position -> Picture
drawHighlight (f, r) =
  translate (fromIntegral f * squareSize - offset + squareSize/2)
            (fromIntegral r * squareSize - offset + squareSize/2) $
  G.color (makeColor 0.0 1.0 0.0 0.5) $
  rectangleWire (squareSize * 1.05) (squareSize * 1.05)

drawMoveHighlight :: Position -> G.Color -> Picture
drawMoveHighlight (f, r) highlightColor =
  translate (fromIntegral f * squareSize - offset + squareSize/2)
            (fromIntegral r * squareSize - offset + squareSize/2) $
  G.color highlightColor $
  circleSolid (squareSize / 4)

-- User input events
handleEvent :: Event -> GlossGame -> GlossGame
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) game =
  let gs = glossGameState game
      board = gameBoard gs
      curPlayer = currentPlayer gs
      
      -- Convert screen coordinates to board position
      clickPos = (floor ((x + offset) / squareSize), floor ((y + offset) / squareSize))
      
      -- Skip in these cases
      shouldSkip = not (isValidPosition clickPos) || 
                   isGameOver gs || 
                   isInCheckmatePiece board curPlayer || 
                   isAITurn game
  in
    if shouldSkip then game
    else processClick game clickPos
handleEvent _ game = game

-- Process mouse clicks on the board
processClick :: GlossGame -> Position -> GlossGame
processClick game clickPos =
  let gs = glossGameState game
      board = gameBoard gs
      curPlayer = currentPlayer gs
  in
    case glossSelected game of
      Nothing -> 
        -- Select a piece if appropriate
        case getPiece board clickPos of
          Just piece | CP.pieceColor piece == curPlayer ->
            game { glossSelected = Just clickPos }
          _ -> game
      
      Just selectedPos ->
        if selectedPos == clickPos then
          -- Deselect if clicked again
          game { glossSelected = Nothing }
        else
          -- Try to move
          let legalMoves = getLegalMoves gs
              matchingMoves = [m | m <- legalMoves, moveFrom m == selectedPos, moveTo m == clickPos]
          in
            case matchingMoves of
              [] -> game { glossSelected = Nothing }
              (move:_) ->
                let newGameState = makeGameMove gs move
                    opponentColor = CP.opponent curPlayer
                    newBoard = gameBoard newGameState
                    checkmateDetected = isInCheckmatePiece newBoard opponentColor
                    
                    moveDesc = case moveType move of
                      Castle True -> "castled kingside"
                      Castle False -> "castled queenside"
                      EnPassant -> "captured en passant"
                      _ -> "moved " ++ moveToString move
                    
                    newMessage = if checkmateDetected then "Checkmate! You win!"
                               else if isGameOver newGameState then "Game over: " ++ show (getGameResult newGameState)
                               else case glossAIPlayer game of
                                      Just _ -> "AI thinking..."
                                      Nothing -> show (currentPlayer newGameState) ++ " to move"
                in
                  game { glossGameState = newGameState
                       , glossSelected = Nothing
                       , glossMessage = moveDesc ++ ". " ++ newMessage
                       }