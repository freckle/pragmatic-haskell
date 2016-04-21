module FooParser where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

spaceConsumer :: Parser ()
spaceConsumer = L.space (void $ spaceChar)
                        (L.skipLineComment "//")
                        (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

word :: Parser String
word = lexeme $ char '<' *> ((:) <$> letterChar <*> many (alphaNumChar <|> char '-')) <* char '>'

paragraph :: Parser [String]
paragraph = many word <* lexeme (string "%%%%")

paragraphs :: Parser [[String]]
paragraphs = spaceConsumer *> many paragraph <* eof
