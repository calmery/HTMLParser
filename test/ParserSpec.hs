module ParserSpec (spec) where

import           Parser     (Attribute (..), Attributes (..), Element (..),
                             Elements (..), parse)
import           Test.Hspec

spec :: Spec
spec = describe "HTML" $ it "" $ do
  parse "<p></p>" `shouldBe`
    show (Elements [Node "p" (Attributes []) (Elements [])])
  parse "<p>Hello World</p>" `shouldBe`
    show (Elements [Node "p" (Attributes []) (Elements [Text "Hello World"])])
  parse "<p><p>Hello World</p></p>" `shouldBe`
    show (Elements [Node "p" (Attributes []) (Elements [Node "p" (Attributes []) (Elements [Text "Hello World"])])])
  parse "<div>Hello World</div>" `shouldBe`
    show (Elements [Node "div" (Attributes []) (Elements [Text "Hello World"])])
  parse "<div><div>Hello World</div></div>" `shouldBe`
    show (Elements [Node "div" (Attributes []) (Elements [Node "div" (Attributes []) (Elements [Text "Hello World"])])])
  parse "<p id=\"id\" class=\"class\"><p>Hello World</p></p>" `shouldBe`
    show (Elements [Node "p" (Attributes [Attribute "id" "id", Attribute "class" "class"]) (Elements [Node "p" (Attributes []) (Elements [Text "Hello World"])])])
  parse "<p id=\"id\" class=\"class\"><p id=\"id\" class=\"class\">Hello World</p></p>" `shouldBe`
    show (Elements [Node "p" (Attributes [Attribute "id" "id", Attribute "class" "class"]) (Elements [Node "p" (Attributes [Attribute "id" "id", Attribute "class" "class"]) (Elements [Text "Hello World"])])])
  parse "<p></p><p></p>" `shouldBe`
    show (Elements [Node "p" (Attributes []) (Elements []), Node "p" (Attributes []) (Elements [])])
  parse "<p><p></p><p></p></p><p></p>" `shouldBe`
    show (Elements [Node "p" (Attributes []) (Elements [Node "p" (Attributes []) (Elements []), Node "p" (Attributes []) (Elements [])]), Node "p" (Attributes []) (Elements [])])
