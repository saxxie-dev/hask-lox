module Main where

import System.Environment
import System.IO
import Scanner
import LoxState

main :: IO ()
main = do
  args <- getArgs
  if (length args) == 0 then do 
    runPromptWithState initialLoxState
  else 
    runFiles args

runFiles :: [String] -> IO ()
runFiles x = do 
  contents <- readFile (x !! 0)
  interpretLines (lines contents) initialLoxState
  

runPromptWithState :: LoxState -> IO ()
runPromptWithState state = do 
  putStr "> "
  hFlush stdout
  line <- getLine
  if line == "exit" 
    then return ()
    else do 
      newState <- interpretLine line state
      runPromptWithState newState

interpretLine :: String -> LoxState -> IO LoxState
interpretLine _ state = return state

interpretLines :: [String] -> LoxState -> IO LoxState
interpretLines [] state = (return state)
interpretLines (l : ls) state = do
  newState <- interpretLine l state 
  interpretLines ls newState