module Main where

import Entities
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Text.Megaparsec
import FooParser (parseSections, Section(..))
import qualified Database.Esqueleto as E

main :: IO ()
main = do
  fileContents <- readFile "specs.foo"

  -- parse specs.foo
  let sections = case runParser parseSections "specs.foo" fileContents of
        Left err        -> error $ "could not parse .foo file with error: " ++ show err
        Right sections' -> sections'

  -- run everything inside single connection/txn against an in-memory
  -- instance of SQLite
  runSqlite ":memory:" $ do

    -- run a safe migration
    runMigration migrateAll

    -- prepare users, insert users, get entities
    let users = map (\section -> User $ sectionUsername section) sections :: [User]
    _ <- insertMany users

    -- fetch all users (note GHC type hint here)
    userEntities <- selectList ([] :: [Filter User]) []
    liftIO $ putStrLn ""
    liftIO $ putStrLn "Users:"
    liftIO $ print userEntities

    -- let's fetch all users with filter, (questionably useful) limit and sorting
    userEntities' <- selectList ([UserName ==. "alex"]) [LimitTo 1, Asc UserName]
    liftIO $ putStrLn ""
    liftIO $ putStrLn "Users:"
    liftIO $ print userEntities'

    -- prepare paragraphs, insert pagaraphs
    let paragraphs = concat $
                     [ map (\pars -> Paragraph (unwords pars) (entityKey u)) (sectionParagraphs s)
                     | u <- userEntities
                     , s <- sections
                     , userName (entityVal u) == sectionUsername s ]

    _ <- insertMany paragraphs

    allParags <- selectList ([] :: [Filter Paragraph]) []
    liftIO $ putStrLn ""
    liftIO $ putStrLn "All Paragraphs:"
    liftIO $ print allParags

    -- uh oh.. how do I join? esqueleto to the rescue
    alexParags <- E.select $
                  E.from $ \(users `E.LeftOuterJoin` paragraphs) -> do
                  E.on ((users E.^. UserId) E.==. (paragraphs E.^.ParagraphUserId))
                  E.where_ ((users E.^. UserName) E.==. (E.val "alex"))
                  return paragraphs

    liftIO $ putStrLn ""
    liftIO $ putStrLn "Alex's Paragraphs:"
    liftIO $ print alexParags

    -- we can compose! Define fn giving us alex where clause
    let whereClause table = E.where_ ((table E.^. UserName) E.==. (E.val "alex"))

    alexFirstParag <- E.select $
                      E.from $ \(users `E.LeftOuterJoin` paragraphs) -> do
                      E.on ((users E.^. UserId) E.==. (paragraphs E.^.ParagraphUserId))
                      (whereClause users) -- can reuse this!
                      E.limit 1
                      return paragraphs

    liftIO $ putStrLn ""
    liftIO $ putStrLn "Alex's First Paragraph:"
    liftIO $ print alexFirstParag

    liftIO $ putStrLn ""
    liftIO $ print "--- all done ---"

--     -- TODO: tests against DB perhaps?
