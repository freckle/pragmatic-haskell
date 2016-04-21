module FooParserSpec (main, spec) where

import Test.Hspec
import FooParser (parse)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "do stuff" $
    it "tests a thing" $ do
      1 `shouldBe` 2
      parse "foo" `shouldBe` "foo"
