module Main where

import Entities
import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Text.Megaparsec
import FooParser (parseSections, Section(..))
import Data.List (unwords)

main :: IO ()
main = do
  fileContents <- readFile "specs.foo"

  -- could be pure but we're using print so we'll get an IO back
  sections <- case runParser parseSections "specs.foo" fileContents of
        Left err -> do
          print err
          error "could not parse .foo file"
        Right sections' -> return sections'


  runSqlite ":memory:" $ do
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

    liftIO $ print "all done"

    -- TODO: talk about TH
    -- TODO: something interesting in esqueleto, preferably composable
    -- TODO: tests
