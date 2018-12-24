module Parser
  (parse, HTML(..), Attribute(..)) where

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

data HTML
  = Tag String [Attribute] [HTML]
  | Text String
  deriving Show

data Attribute
  = Attribute String String
  deriving Show

expression :: Parser [HTML]
expression = many $ try tag <|> text
  where
    tag = do
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
      return $ Tag (s:t) a c

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

text :: Parser HTML
text = do
  s <- many1 $ letter <|> space
  return $ Text s
