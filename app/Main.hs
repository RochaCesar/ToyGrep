module Main (main) where

import Data.Char
import Data.List
import Lib
import System.Environment
import System.Exit
import System.IO (BufferMode (NoBuffering), hPutStrLn, hSetBuffering, stderr, stdout)

main :: IO ()
main = do
  -- Disable output buffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  args <- getArgs
  let pattern = args !! 1
  input_line <- getLine

  -- You can use print statements as follows for debugging, they'll be visible when running tests.
  hPutStrLn stderr "Logs from your program will appear here"

  -- Uncomment this block to pass stage 1
  if head args /= "-E"
    then do
      putStrLn "Expected first argument to be '-E'"
      exitFailure
    else do
      if matchPattern pattern input_line
        then exitSuccess
        else exitFailure
