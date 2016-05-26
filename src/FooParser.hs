module FooParser where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L

data Section = Section { sectionUsername :: String
                       , sectionParagraphs :: [[String]]
                       } deriving (Eq, Show)

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

parseLexeme :: Parser a -> Parser a
parseLexeme = L.lexeme spaceConsumer

parseWord :: Parser String
parseWord = parseLexeme $ do
  _ <- char '<'
  word <- some (alphaNumChar <|> char '-')
  _ <- char '>'
  return word

parseWords :: Parser [String]
parseWords = some parseWord

parseParagraphs :: Parser [[String]]
parseParagraphs = parseWords `sepBy` parseLexeme (string "%%%%")

parseSection :: Parser Section
parseSection = do
  username' <- parseLexeme $ some alphaNumChar
  _ <- parseLexeme $ char ':'
  _ <- parseLexeme $ char '{'
  paragraphs' <- parseParagraphs
  _ <- parseLexeme $ char '}'
  return $ Section username' paragraphs'

parseSections :: Parser [Section]
parseSections = some parseSection
