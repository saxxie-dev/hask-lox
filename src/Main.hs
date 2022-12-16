module Main where

import System.Environment
import Scanner

main :: IO ()
main = do
  args <- getArgs
  if (length args) == 0 then
    runPrompt
  else 
    runFiles args


runFiles :: [String] -> IO ()
runFiles x = putStrLn (x !! 0)

runPrompt :: IO ()
runPrompt = putStrLn "hello world"
