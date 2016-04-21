module Main where

import System.Environment (getEnv)
import Text.Megaparsec (runParser)
import Data.Monoid ((<>))
import FooParser
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Message
  content String
|]


main :: IO ()
main = do
  let filename = "safemessage.foo"
  fileContent <- readFile filename
  let parsedParagraphs = case runParser paragraphs filename fileContent of
        Left err -> error $ "ERROR: " <> show err
        Right res -> res

  runSqlite "mydb.sqlite" $ do
    runMigration migrateAll

  print "done"
