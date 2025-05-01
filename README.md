# Chess Bot

INF221 The Project haskell Chess Bot

## Features

- Chess engine with move generation and evaluation
- Game logic handling turns, legal moves, and checkmate detection
- AI opponent using Minimax with Alpha-Beta pruning with different difficulty
- Graphical UI with gloss
- FEN notation support for loading/saving positions
- Castling, En passant, Promotion, 50-move rule and stalemate.

## Building and Running

### Prerequisites

- GHC 
- Stack

### Building

```bash
stack build
```

### Running

```bash
stack exec chess-bot
```

### Testing

```bash
stack test
```

## Project Structure

- `src/Chess/Board.hs` - Board representation and core functionality
- `src/Chess/Pieces.hs` - Chess piece definitions and movement rules
- `src/Chess/Move.hs` - Move generation and validation
- `src/Chess/Game.hs` - Game state management
- `src/Chess/AI/Minimax.hs` - AI implementation with Minimax algorithm
- `src/Chess/FEN.hs` - Forsyth-Edwards Notation parser/generator
- `src/Chess/Render/Gloss.hs` - GUI-based rendering of the chess board
- `/images` - Chess pieces



