module FooParser where

import Control.Monad (void)
-- import Data.Text
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import Data.Monoid ((<>))

data Word = LessThan | Content | GreaterThan
data Paragraph = ParagraphContent | EndOfParagraph

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar)
                        (L.skipLineComment "//")
                        (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

brackets :: Parser a -> Parser a
brackets = between (symbol "<") (symbol ">")

word :: Parser String
word = lexeme ((:) <$> letterChar <*> many alphaNumChar)

-- parse :: String -> Parser String
-- parse = undefined

runMain :: IO ()
runMain = do
  -- case runParser word "" "foo   asdf" of
  --         Left err ->  print $ "ERROR: " <> show err
  --         Right res -> print $ "RESULT: " <> res
  case runParser (brackets word) "" "<foo>" of
          Left err ->  print $ "ERROR: " <> show err
          Right res -> print $ "RESULT: " <> res
