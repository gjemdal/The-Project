module Chess.MoveSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromJust)

import Chess.Board
import Chess.Pieces
import Chess.Move
import Chess.Game

spec :: Spec
spec = do
  describe "Move generation" $ do
    it "generates pawn moves correctly" $ do
      let board = initialBoard
          pawnMoves = filter (\move -> pieceType (movingPiece move) == Pawn) 
                             (generateMoves board White)
      -- Verify that there are 16 possible pawn moves from starting position (8 pawns * 2 moves each)
      length pawnMoves `shouldBe` 16
    
    it "generates knight moves correctly" $ do
      let board = initialBoard
          knightMoves = filter (\move -> pieceType (movingPiece move) == Knight) 
                               (generateMoves board White)
      -- In starting position, each knight can move to 2 squares
      length knightMoves `shouldBe` 4
    
    it "generates no legal moves for bishops in initial position" $ do
      let board = initialBoard
          bishopMoves = filter (\move -> pieceType (movingPiece move) == Bishop) 
                               (generateMoves board White)
      -- Bishops are blocked in the initial position
      length bishopMoves `shouldBe` 0
    
    it "generates no legal moves for queens in initial position" $ do
      let board = initialBoard
          queenMoves = filter (\move -> pieceType (movingPiece move) == Queen) 
                              (generateMoves board White)
      -- Queen is blocked in the initial position
      length queenMoves `shouldBe` 0
  
  describe "Move application" $ do
    it "applies pawn moves correctly" $ do
      let board = initialBoard
          -- Move a pawn from e2 to e4
          e2 = fromJust $ stringToPosition "e2"
          e4 = fromJust $ stringToPosition "e4"
          pawn = fromJust $ getPiece board e2
          move = Move e2 e4 Normal pawn Nothing
          newBoard = makeMove board move
      
      getPiece newBoard e2 `shouldBe` Nothing
      getPiece newBoard e4 `shouldBe` Just pawn
    
    it "handles captures correctly" $ do
      -- Create a board with a white pawn that can capture a black piece
      let e4 = fromJust $ stringToPosition "e4"
          d5 = fromJust $ stringToPosition "d5"
          whitePawn = Piece Pawn White
          blackPawn = Piece Pawn Black
          board = setPiece (setPiece emptyBoard e4 whitePawn) d5 blackPawn
          move = Move e4 d5 Capture whitePawn (Just blackPawn)
          newBoard = makeMove board move
      
      getPiece newBoard e4 `shouldBe` Nothing
      getPiece newBoard d5 `shouldBe` Just whitePawn
    
    it "validates moves correctly" $ do
      let board = initialBoard
          -- Valid move: e2 to e4
          e2 = fromJust $ stringToPosition "e2"
          e4 = fromJust $ stringToPosition "e4"
          pawn = fromJust $ getPiece board e2
          validMove = Move e2 e4 Normal pawn Nothing
          
          -- Invalid move: e2 to e5 (too far)
          e5 = fromJust $ stringToPosition "e5"
          invalidMove = Move e2 e5 Normal pawn Nothing
      
      isLegalMove board validMove `shouldBe` True
      isLegalMove board invalidMove `shouldBe` False
  
  describe "Check detection" $ do
    it "detects check correctly" $ do
      -- Create a board with white king in check
      let e1 = fromJust $ stringToPosition "e1"
          e8 = fromJust $ stringToPosition "e8"
          a5 = fromJust $ stringToPosition "a5"
          
          whiteKing = Piece King White
          blackQueen = Piece Queen Black
          blackKing = Piece King Black
          
          board = setPiece (setPiece (setPiece emptyBoard e1 whiteKing) e8 blackKing) a5 blackQueen
      
      isCheck board White `shouldBe` True  -- White king is in check
      isCheck board Black `shouldBe` False  -- Black king is not in check