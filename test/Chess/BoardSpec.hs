module Chess.BoardSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Chess.Board
import Chess.Pieces

validPosition :: Gen Position
validPosition = do
  f <- choose (0, 7)
  r <- choose (0, 7)
  return (f, r)

spec :: Spec
spec = do
  describe "Board operations" $ do
    it "creates an empty board" $ do
      getPiece emptyBoard (0, 0) `shouldBe` Nothing
    
    it "sets and gets pieces correctly" $ do
      let piece = Piece Rook White
          board = setPiece emptyBoard (3, 3) piece
      getPiece board (3, 3) `shouldBe` Just piece
      getPiece board (0, 0) `shouldBe` Nothing
    
    it "removes pieces correctly" $ do
      let piece = Piece Queen Black
          board = setPiece emptyBoard (5, 6) piece
          boardAfterRemove = removePiece board (5, 6)
      getPiece board (5, 6) `shouldBe` Just piece
      getPiece boardAfterRemove (5, 6) `shouldBe` Nothing
    
    it "moves pieces correctly" $ do
      let piece = Piece Bishop White
          board = setPiece emptyBoard (2, 1) piece
          boardAfterMove = movePiece board (2, 1) (4, 3)
      getPiece board (2, 1) `shouldBe` Just piece
      getPiece board (4, 3) `shouldBe` Nothing
      getPiece boardAfterMove (2, 1) `shouldBe` Nothing
      getPiece boardAfterMove (4, 3) `shouldBe` Just piece
  
  describe "Position utils" $ do
    it "validates positions correctly" $ do
      isValidPosition (0, 0) `shouldBe` True
      isValidPosition (7, 7) `shouldBe` True
      isValidPosition (-1, 0) `shouldBe` False
      isValidPosition (0, 8) `shouldBe` False
    
    it "converts positions to strings and back" $
      forAll validPosition $ \pos ->
        stringToPosition (positionToString pos) == Just pos
    
    it "converts files and ranks to chars and back" $
      forAll validPosition $ \(f, r) ->
        charToFile (fileToChar f) == Just f &&
        charToRank (rankToChar r) == Just r
  
  describe "Initial board setup" $ do
    it "initializes pieces in correct positions" $ do
      -- Check a few key pieces
      getPiece initialBoard (0, 0) `shouldBe` Just (Piece Rook White)
      getPiece initialBoard (1, 0) `shouldBe` Just (Piece Knight White)
      getPiece initialBoard (4, 0) `shouldBe` Just (Piece King White)
      getPiece initialBoard (3, 0) `shouldBe` Just (Piece Queen White)
      
      getPiece initialBoard (0, 7) `shouldBe` Just (Piece Rook Black)
      getPiece initialBoard (1, 7) `shouldBe` Just (Piece Knight Black)
      getPiece initialBoard (4, 7) `shouldBe` Just (Piece King Black)
      getPiece initialBoard (3, 7) `shouldBe` Just (Piece Queen Black)
      
      -- Check pawns
      getPiece initialBoard (0, 1) `shouldBe` Just (Piece Pawn White)
      getPiece initialBoard (7, 1) `shouldBe` Just (Piece Pawn White)
      getPiece initialBoard (0, 6) `shouldBe` Just (Piece Pawn Black)
      getPiece initialBoard (7, 6) `shouldBe` Just (Piece Pawn Black)
      
      -- Check empty squares
      getPiece initialBoard (0, 2) `shouldBe` Nothing
      getPiece initialBoard (7, 5) `shouldBe` Nothing