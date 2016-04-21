module FooParserSpec (main, spec) where

import Test.Hspec
import FooParser
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import Data.Either

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parse stuff" $ do
    it "can parse a word" $ do
      runParser word "" "<foo>" `shouldBe` Right "foo"
    it "can parse word with empty space" $ do
      runParser word "" "<foo>     s" `shouldBe` Right "foo"
    it "can parse word with dashes" $ do
      runParser word "" "<foo-bar>" `shouldBe` Right "foo-bar"
    it "can parse many words" $ do
      runParser paragraph "" "<foo> <bar> %%%%" `shouldBe` Right ["foo", "bar"]
    it "can parse many bracketed" $ do
      runParser paragraph "" "<foo> <bar> %%%%" `shouldBe` Right ["foo", "bar"]
    it "can parse many paragraphs" $ do
      runParser paragraphs "" "<foo> %%%% <bar> %%%%" `shouldBe` Right [["foo"], ["bar"]]
    it "can parse many paragraphs" $ do
      runParser paragraphs "" "<foo> %%%% <bar> %%%%" `shouldBe` Right [["foo"], ["bar"]]
    it "must consume EVERYTHING" $ do
      runParser paragraphs "" "<foo> %%%% 123" `shouldSatisfy` isLeft

