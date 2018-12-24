module ParserSpec (spec) where

import           Parser     (HTML (..), parse)
import           Test.Hspec

spec :: Spec
spec = describe "HTML" $ it "" $ do
  parse "<p>Hello World</p>" `shouldBe` (show $ Tag "p" (Children "Hello World"))
  parse "<p><p>Hello World</p></p>" `shouldBe` (show $ Tag "p" (Tag "p" (Children "Hello World")))
  parse "<div>Hello World</div>" `shouldBe` (show $ Tag "div" (Children "Hello World"))
  parse "<div><div>Hello World</div></div>" `shouldBe` (show $ Tag "div" (Tag "div" (Children "Hello World")))
