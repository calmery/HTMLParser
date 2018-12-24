module Main where

import           Parser (parse)

main :: IO ()
main = loop
  where
    loop = do
      input <- getLine
      putStrLn $ parse input
      loop
