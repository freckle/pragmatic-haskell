module FooParser where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import Prelude hiding (words)

data UserSection = UserSection String Section
                   deriving (Eq, Show)

data Section = Section [[String]]
               deriving (Eq, Show)

spaceConsumer :: Parser ()
spaceConsumer = L.space (void $ spaceChar)
                        (L.skipLineComment "//")
                        (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

word :: Parser String
word = lexeme $ do
  _ <- char '<'
  all <- some (alphaNumChar <|> char '-')
  _ <- char '>'
  return all

paragraph :: Parser [String]
paragraph = do
  words <- some word
  return words

paragraphs :: Parser [[String]]
paragraphs = do
  paragraphs' <- paragraph `sepBy1` lexeme (string "%%%%")
  return paragraphs'

section :: Parser Section
section = do
  _ <- lexeme $ char '{'
  p' <- paragraphs
  _ <- lexeme $ char '}'
  return $ Section p'

userSection :: Parser UserSection
userSection = do
  username <- lexeme $ some alphaNumChar
  _ <- lexeme $ char ':'
  s' <- section
  return $ UserSection username s'

userSections :: Parser [UserSection]
userSections = do
  spaceConsumer
  usses <- some userSection
  eof
  return usses
