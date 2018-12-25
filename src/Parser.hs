module Parser
  (parse, Attribute(..), Attributes(..), Element(..), Elements(..)) where

import           Control.Applicative (pure, (*>))
import           Text.Parsec         (many, many1, skipMany, try, (<|>))
import qualified Text.Parsec         as Parsec
import           Text.Parsec.Char    (char, digit, letter, newline, oneOf,
                                      space, string, tab)
import           Text.Parsec.String  (Parser)

parse :: String -> String
parse input =
  case Parsec.parse expression "HTML" input of
    Left error ->
      show error

    Right formula ->
      show formula

-- Elements

newtype Elements
  = Elements [Element]
  deriving Show

data Element
  = Node String Attributes Elements
  | Text String
  deriving Show

-- Attributes

newtype Attributes = Attributes [Attribute]
  deriving Show

data Attribute
  = Attribute String String
  deriving Show

-- Parser

expression :: Parser Elements
expression = do
  elements <- many $ try node <|> text
  pure $ Elements elements

node :: Parser Element
node = do
  skipMany $ newline <|> tab <|> space
  char '<'
  h <- letter
  t <- many $ letter <|> digit
  let name = h:t
  attributes <- many $ try attribute
  elements <- close name <|> empty
  skipMany $ newline <|> tab <|> space
  pure $ Node name (Attributes attributes) elements
  where
    close name = do
      char '>'
      elements <- expression
      char '<' *> char '/' *> string name *> char '>'
      return elements
    empty = do
      skipMany space
      char '/'
      char '>'
      return $ Elements []

text :: Parser Element
text = do
  t <- many1 $ letter <|> space <|> newline
  pure $ Text t

attribute :: Parser Attribute
attribute = do
  space
  h <- letter
  t <- many $ letter <|> digit
  let name = h:t
  char '=' *> char '\"'
  value <- many $ letter <|> digit <|> oneOf ['/', ':', '.'] <|> space
  char '\"'
  pure $ Attribute name value
