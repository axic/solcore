module Main where

import Solcore.Frontend.Parser.SolcoreParser (moduleParser)
import Solcore.Frontend.Pretty.SolcorePretty (pretty)
import Solcore.Frontend.Syntax.NameResolution (nameResolution)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> prettyPrintFile path
    _ -> do
      progName <- getProgName
      hPutStrLn stderr ("Usage: " ++ progName ++ " <path/filename>")
      exitFailure

prettyPrintFile :: FilePath -> IO ()
prettyPrintFile path = do
  content <- readFile path
  parsed <- moduleParser [takeDirectory path] content
  case parsed of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right cu -> do
      resolved <- nameResolution cu
      case resolved of
        Left err -> do
          hPutStrLn stderr err
          exitFailure
        Right cu' -> putStrLn (pretty cu')
