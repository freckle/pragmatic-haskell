module Main where

import System.Environment (getEnv)
import Text.Megaparsec (runParser)
import Data.Monoid ((<>))
import FooParser
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
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

    messagesE <- fmap concat $ forM usses $ \(UserSection username (Section sect)) -> do
      let ue = fromMaybe (error "user not found")
             $ find (\u -> userName (entityVal u) == username) usersE
          stitched = map (\listWords -> concat $ intersperse " " listWords) sect
      forM stitched (\message -> insert $ Message (entityKey ue) message)

    u' <- selectList ([] :: [Filter User]) []
    liftIO $ print u'

    m' <- selectList ([] :: [Filter Message]) []
    liftIO $ print m'

    

    return ()
  print "done"
