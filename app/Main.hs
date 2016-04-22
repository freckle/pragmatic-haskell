module Main where

import System.Environment (getEnv)
import Text.Megaparsec (runParser)
import Data.Monoid ((<>))
import FooParser
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import Data.Traversable
import Data.Maybe
import Data.List (find, intersperse)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User sql=users
  name String
  UniqueName name
  deriving Show Eq
Message sql=messages
  userId UserId
  content String
  deriving Show Eq
|]

main :: IO ()
main = do
  let filename = "newmessages.foo"
  fileContent <- readFile filename
  let usses = case runParser userSections filename fileContent of
        Left err -> error $ "ERROR: " <> show err
        Right res -> res
      users = map (\(UserSection u _) -> User u) usses

  runSqlite "mydb.sqlite" $ do
    runMigration migrateAll
    usersE <- forM users (\u -> upsert u [])

    _ <- forM usses $ \(UserSection username (Section sect)) -> do
      let ue = fromMaybe (error "user not found")
             $ find (\u -> userName (entityVal u) == username) usersE
          -- don't do this in the real world, use Safe
          stitched = head $ map (\listWords -> concat $ intersperse " " listWords) sect
      liftIO $ print stitched

      -- forM sect (\s -> insert $ Message (entityKey ue) s)

    -- liftIO $ print messages
    return ()

  print "done"
