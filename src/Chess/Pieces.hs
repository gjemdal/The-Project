module Chess.Pieces
  ( Piece(..)
  , PieceType(..)
  , Color(..)
  , pieceValue
  , pieceToChar
  , charToPiece
  , opponent
  ) where

data Color = White | Black
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Piece = Piece
  { pieceType :: PieceType
  , pieceColor :: Color
  } deriving (Eq, Ord, Show, Read)

opponent :: Color -> Color
opponent White = Black
opponent Black = White

-- Standard material value for each piece type
pieceValue :: PieceType -> Int
pieceValue Pawn   = 100
pieceValue Knight = 300
pieceValue Bishop = 325
pieceValue Rook   = 500
pieceValue Queen  = 900
pieceValue King   = 10000

-- | Convert a piece type to its character representation (uppercase)
typeToChar :: PieceType -> Char
typeToChar Pawn   = 'P'
typeToChar Knight = 'N'
typeToChar Bishop = 'B'
typeToChar Rook   = 'R'
typeToChar Queen  = 'Q'
typeToChar King   = 'K'

-- Uppercase for white pieces, lowercase for black
pieceToChar :: Piece -> Char
pieceToChar (Piece pt White) = typeToChar pt
pieceToChar (Piece pt Black) = toLower (typeToChar pt)
  where    
    toLower :: Char -> Char
    toLower c = case c of
      'P' -> 'p'
      'N' -> 'n'
      'B' -> 'b'
      'R' -> 'r'
      'Q' -> 'q'
      'K' -> 'k'
      _   -> c

-- Convert a character to a piece (if valid)
charToPiece :: Char -> Maybe Piece
charToPiece c = case c of
  'P' -> Just $ Piece Pawn White
  'N' -> Just $ Piece Knight White
  'B' -> Just $ Piece Bishop White
  'R' -> Just $ Piece Rook White
  'Q' -> Just $ Piece Queen White
  'K' -> Just $ Piece King White
  'p' -> Just $ Piece Pawn Black
  'n' -> Just $ Piece Knight Black
  'b' -> Just $ Piece Bishop Black
  'r' -> Just $ Piece Rook Black
  'q' -> Just $ Piece Queen Black
  'k' -> Just $ Piece King Black
  _   -> Nothing