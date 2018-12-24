module Parser
  (parse, HTML(..)) where

import           Text.Parsec        (many, (<|>))
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
  = Tag String HTML
  | Children String
  deriving Show

expression :: Parser HTML
expression = tag <|> children
  where
    tag = do
      char '<'
      s <- letter
      t <- many $ letter <|> digit
      char '>'
      c <- expression
      do
        char '<'
        char '/'
        string (s:t)
        char '>'
      return $ Tag (s:t) c
    children = do
      s <- many $ letter <|> space
      return $ Children s
