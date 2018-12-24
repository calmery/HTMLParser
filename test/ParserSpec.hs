module ParserSpec (spec) where

import           Parser     (Attribute (..), HTML (..), parse)
import           Test.Hspec

spec :: Spec
spec = describe "HTML" $ it "" $ do
  parse "<p>Hello World</p>" `shouldBe`
    show (Tag "p" [] (Text "Hello World"))
  parse "<p><p>Hello World</p></p>" `shouldBe`
    show (Tag "p" [] (Tag "p" [] (Text "Hello World")))
  parse "<div>Hello World</div>" `shouldBe`
    show (Tag "div" [] (Text "Hello World"))
  parse "<div><div>Hello World</div></div>" `shouldBe`
    show (Tag "div" [] (Tag "div" [] (Text "Hello World")))
  parse "<p id=\"id\" class=\"class\"><p>Hello World</p></p>" `shouldBe`
    show (Tag "p" [Attribute "id" "id", Attribute "class" "class"] (Tag "p" [] (Text "Hello World")))
  parse "<p id=\"id\" class=\"class\"><p id=\"id\" class=\"class\">Hello World</p></p>" `shouldBe`
    show (Tag "p" [Attribute "id" "id", Attribute "class" "class"] (Tag "p" [Attribute "id" "id", Attribute "class" "class"] (Text "Hello World")))
