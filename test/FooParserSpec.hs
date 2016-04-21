module FooParserSpec (main, spec) where

import Test.Hspec
import FooParser
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parse stuff" $ do
    it "can parse a word" $ do
      runParser word "" "foo" `shouldBe` Right "foo"
    it "can parse word with empty space" $ do
      runParser word "" "foo     s" `shouldBe` Right "foo"
    it "can parse word with brackets around it" $ do
      runParser (brackets word) "" "<foo>" `shouldBe` Right "foo"
    it "can parse several words with brackets" $ do
      pending

