module Main where

import System.Environment (getEnv)
import Text.Megaparsec
import Data.Monoid ((<>))
import FooParser

main :: IO ()
main = do
  filename <- getEnv "FILENAME"
  fileContent <- readFile filename
  case runParser paragraphs filename fileContent of
          Left err ->  print $ "ERROR: " <> show err
          Right res -> do
            print $ "RESULT: "
            print res
