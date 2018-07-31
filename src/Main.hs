{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Parsing
import Token


data Error
  = ParsingError Int String

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> usage
    1 -> runFile $ head args
    _ -> runPrompt

usage :: IO ()
usage = do
  putStrLn "Usage: jlox [script]"
  exitWith $ ExitFailure 64

runFile :: FilePath -> IO ()
runFile file = do
  bs <- BS.readFile file
  putStrLn "DOM"

runPrompt :: IO ()
runPrompt = do
  line <- BS.getLine
  run line
  runPrompt

run :: BS.ByteString -> IO ()
run bytes = putStrLn "Running run"

error :: Int -> String -> Error
error = ParsingError

renderError :: Error -> String
renderError (ParsingError line message) = "[line " ++ show line ++ "] Error: " ++ message;
  