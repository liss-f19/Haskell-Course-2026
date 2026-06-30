module Main (main) where

import MathLang
import System.Environment (getArgs)
import System.Directory (doesFileExist)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fp] -> runFile fp
    _ -> putStrLn "Usage: mathlang-exe <program-file>"

runFile :: FilePath -> IO ()
runFile fp = do
  exists <- doesFileExist fp
  if not exists
    then putStrLn ("Input file not found: " <> fp)
    else do
      src <- readFile fp
      case parseProgram src of
        Left err -> putStrLn ("Parse error:\n" <> err)
        Right program ->
          case evalProgram program of
            Left e -> putStrLn ("Evaluation error: " <> show e)
            Right v -> print v
