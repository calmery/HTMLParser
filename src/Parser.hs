module Parser
  (parse, Attribute(..), Attributes(..), Element(..), Elements(..)) where

import           Text.Parsec        (many, many1, try, (<|>))
import qualified Text.Parsec        as Parsec
import           Text.Parsec.Char   (char, digit, letter, space, string)
import           Text.Parsec.String (Parser)

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
  c <- many $ try node <|> text
  return $ Elements c

node :: Parser Element
node = do
  char '<'
  s <- letter
  t <- many $ letter <|> digit
  a <- many attribute
  char '>'
  c <- expression
  char '<'
  char '/'
  string (s:t)
  char '>'
  return $ Node (s:t) (Attributes a) c

text :: Parser Element
text = do
  s <- many1 $ letter <|> space
  return $ Text s

attribute :: Parser Attribute
attribute = do
  space
  s <- letter
  t <- many $ letter <|> digit
  char '='
  char '\"'
  c <- many $ letter <|> digit <|> space
  char '\"'
  return $ Attribute (s:t) c
