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

  -- run everything inside single connection/txn against a sqlite file
  runSqlite "ourdb.sqlite" $ do

    runMigration migrateAll

    -- prepare users, insert users, get entities. reminder:
    -- Section { sectionUsername :: [String], sectionParagraphs :: [[String]] }
    let users = map (\section -> User $ sectionUsername section) sections :: [User]
    _ <- insertMany users

    -- fetch all users (note GHC type hint here)
    userEntities <- selectList ([] :: [Filter User]) []
    liftIO $ do
      putStrLn ""
      putStrLn "Users:"
      print userEntities

    -- let's fetch all users with filter, (questionably useful) limit and
    -- ordering
    userEntities' <- selectList ([UserName ==. "alex"]) [LimitTo 1, Asc UserName]
    liftIO $ do
      putStrLn ""
      putStrLn "Users:"
      print userEntities'

    -- prepare paragraphs, insert pagaraphs
    let paragraphs = concat $
                     [ map (\pars -> Paragraph (unwords pars) (entityKey u)) (sectionParagraphs s)
                     | u <- userEntities
                     , s <- sections
                     , userName (entityVal u) == sectionUsername s ]
                     :: [Paragraph]

    _ <- insertMany paragraphs

    allParags <- selectList ([] :: [Filter Paragraph]) []
    liftIO $ do
      putStrLn ""
      putStrLn "All Paragraphs:"
      print allParags

    -- uh oh.. how do I join? could do raw sql as a last resort
    let sql = "SELECT ?? \
             \ FROM users \
             \ INNER JOIN paragraphs ON users.id = paragraphs.user_id \
             \ WHERE users.name = ?"
    alexRawParags <- rawSql sql [PersistText "alex"]
    liftIO $ do
      putStrLn ""
      putStrLn "Alex's raw paragraphs:"
      -- GHC has no clue what the raw sql is returning unless I tell it
      print (alexRawParags :: [Entity Paragraph])

    -- eh, that works, but we can do much better. esqueleto to the rescue
    alexParags <- E.select $
                  E.from $ \(users `E.InnerJoin` paragraphs) -> do
                  E.on ((users E.^. UserId) E.==. (paragraphs E.^.ParagraphUserId))
                  E.where_ ((users E.^. UserName) E.==. (E.val "alex"))
                  return paragraphs

    liftIO $ do
      putStrLn ""
      putStrLn "Alex's Paragraphs:"
      print alexParags

    -- we can compose! Define fn giving us alex where clause
    let whereClause table = E.where_ ((table E.^. UserName) E.==. (E.val "alex"))

    alexFirstParag <- E.select $
                      E.from $ \(users `E.LeftOuterJoin` paragraphs) -> do
                      E.on ((users E.^. UserId) E.==. (paragraphs E.^.ParagraphUserId))
                      (whereClause users) -- can reuse this!
                      E.limit 1
                      return paragraphs

    liftIO $ do
      putStrLn ""
      putStrLn "Alex's First Paragraph:"
      print alexFirstParag

    liftIO $ do
      putStrLn ""
      putStrLn "--- all done ---"

-- --     -- TODO: tests against DB perhaps?
