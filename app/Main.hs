module Main where

import Entities
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Text.Megaparsec
import FooParser (parseSections, Section(..))
import Data.List (unwords)
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
    let users = map (\section -> User $ sectionUsername section) sections
    userKeys <- insertMany users
    let userEntities = zipWith (\key user -> Entity key user) userKeys users

    -- prepare paragraphs, insert pagaraphs
        paragraphs = concat $
                     [ map (\pars -> Paragraph (unwords pars) (entityKey u)) (sectionParagraphs s)
                     | u <- userEntities
                     , s <- sections
                     , userName (entityVal u) == sectionUsername s ]

    parKeys <- insertMany paragraphs

    -- let's fetch all users (note GHC type hint here)
    users' <- selectList ([] :: [Filter User]) []
    liftIO $ putStrLn ""
    liftIO $ putStrLn "Users:"
    liftIO $ print users'

    -- let's fetch all users with filter, (questionably useful) limit and sorting
    users'' <- selectList ([UserName ==. "alex"]) [LimitTo 1, Asc UserName]
    liftIO $ putStrLn ""
    liftIO $ putStrLn "Users:"
    liftIO $ print users''

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

    -- TODO: tests against DB perhaps?
