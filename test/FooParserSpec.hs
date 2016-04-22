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
    it "empty string should not be a word" $ do
      runParser word "" "" `shouldSatisfy` isLeft
    it "can parse a word" $ do
      runParser word "" "<foo>" `shouldBe` Right "foo"
    it "can parse word with empty space" $ do
      runParser word "" "<foo>     s" `shouldBe` Right "foo"
    it "can parse word with dashes" $ do
      runParser word "" "<foo-bar>" `shouldBe` Right "foo-bar"
    it "can parse a paragraph" $ do
      runParser paragraph "" "<foo-bar> <baz>" `shouldBe` Right ["foo-bar", "baz"]
    it "can parse paragraphs" $ do
      runParser paragraphs "" "<foo-bar> <baz> %%%% <quux>"
      `shouldBe`
      Right [["foo-bar", "baz"], ["quux"]]
    it "ending delimiter should not parse" $ do
      runParser paragraphs "" "<foo-bar> <baz> %%%% <quux> %%%% "
      `shouldSatisfy`
      isLeft
    it "can parse paragraphs with only one paragraph" $ do
      runParser paragraphs "" "<foo-bar> <baz>"
      `shouldBe`
      Right [["foo-bar", "baz"]]
    it "can parse a section" $ do
      runParser section "" "{ <foo-bar> <baz> }"
      `shouldBe`
      Right (Section [["foo-bar", "baz"]])
    it "can parse a user section" $ do
      runParser userSection "" "alex: { <foo-bar> }"
      `shouldBe`
      Right (UserSection "alex" (Section [["foo-bar"]]))
