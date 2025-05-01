module Main where

import Chess.Pieces
import Chess.Render.Gloss
import System.IO

main :: IO ()
main = do
  putStrLn "Chess Game"
  putStrLn "----------"
  displayMenu
  
  where
    displayMenu = do
      putStrLn "1. Play against another player"
      putStrLn "2. Play against AI (as White)"
      putStrLn "3. Play against AI (as Black)"
      putStrLn "4. Quit"
      putStr "Enter your choice: "
      hFlush stdout
      
      choice <- getLine
      case choice of
        "1" -> runGlossGame
        "2" -> getAIDepth >>= runGlossAI Black
        "3" -> getAIDepth >>= runGlossAI White
        "4" -> putStrLn "Goodbye!"
        _ -> do
          putStrLn "Invalid choice, please try again."
          displayMenu

getAIDepth :: IO Int
getAIDepth = do
  putStrLn "Set AI difficulty:"
  putStrLn "1. Easy (depth 2)"
  putStrLn "2. Medium (depth 3)"
  putStrLn "3. Hard (depth 4)"
  putStr "Enter your choice (1-3): "
  hFlush stdout
  
  choice <- getLine
  return $ case choice of
    "1" -> 2
    "2" -> 3
    "3" -> 4
    _   -> 3  -- Default to medium if invalid input