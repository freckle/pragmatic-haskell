module FooParserSpec where

import Test.Hspec
import FooParser
import Text.Megaparsec

spec :: Spec
spec =
  describe "parses stuff" $ do
    it "parses one word" $ do
      runParser parseWord "" "<foo>" `shouldBe` Right "foo"
    it "parses one word with dash" $ do
      runParser parseWord "" "<foo-bar>" `shouldBe` Right "foo-bar"
    it "parses multiple words" $ do
      runParser parseWords "" "<foo> <bar>" `shouldBe` Right ["foo", "bar"]
    it "should parse paragraphs" $ do
      runParser parseParagraphs "" "<foo> <bar> %%%% <baz>"
      `shouldBe`
      Right [["foo", "bar"], ["baz"]]
    it "should parse section" $ do
      runParser parseSection "" "alex:{<foo> <bar> %%%% <baz>}"
      `shouldBe`
      Right (Section "alex" [["foo", "bar"], ["baz"]])
    it "should parse sections" $ do
      runParser parseSections "" "alex:{<foo> <bar> %%%% <baz>} bob:{<foo>}"
      `shouldBe`
      Right ([Section "alex" [["foo", "bar"], ["baz"]], Section "bob" [["foo"]]])
