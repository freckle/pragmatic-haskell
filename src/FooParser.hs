module FooParser where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import Prelude hiding (words)

spaceConsumer :: Parser ()
spaceConsumer = L.space (void $ spaceChar)
                        (L.skipLineComment "//")
                        (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

word :: Parser String
word = lexeme $ do
  _ <- char '<'
  first <- letterChar
  rest <- many (alphaNumChar <|> char '-')
  _ <- char '>'
  return $ first:rest

paragraph :: Parser [String]
paragraph = do
  words <- many word
  _ <- lexeme (string "%%%%")
  return words

paragraphs :: Parser [[String]]
paragraphs = do
  spaceConsumer
  paragraphs' <- many paragraph
  eof
  return paragraphs'
