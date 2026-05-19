module Main where

import Solcore.Frontend.Parser.SolcoreParser (moduleParser)
import Solcore.Frontend.Pretty.SolcorePretty (pretty)
import Solcore.Frontend.Syntax.NameResolution (nameResolution)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, readFile', stderr)

data Options = Options
  { optInPlace :: !Bool,
    optPath :: !FilePath
  }

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Just opts -> prettyPrintFile opts
    Nothing -> do
      progName <- getProgName
      hPutStrLn stderr ("Usage: " ++ progName ++ " [-i|--in-place] <path/filename>")
      exitFailure

parseArgs :: [String] -> Maybe Options
parseArgs = go False
  where
    go _ [] = Nothing
    go inPlace (a : rest)
      | a == "-i" || a == "--in-place" = go True rest
      | null rest = Just (Options inPlace a)
      | otherwise = Nothing

prettyPrintFile :: Options -> IO ()
prettyPrintFile opts = do
  let path = optPath opts
  content <- readFile' path
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
        Right cu'
          | optInPlace opts -> writeFile path (pretty cu' ++ "\n")
          | otherwise -> putStrLn (pretty cu')
