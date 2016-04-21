module FooParser where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

spaceConsumer :: Parser ()
spaceConsumer = L.space (void $ spaceChar <|> char '<' <|> char '>')
                        (L.skipLineComment "//")
                        (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

word :: Parser String
word = spaceConsumer
    *> (lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> char '-')))
    <* spaceConsumer

paragraph :: Parser [String]
paragraph = many word <* string "%%%%"

paragraphs :: Parser [[String]]
paragraphs = many paragraph
