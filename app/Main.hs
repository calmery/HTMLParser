module Main where

import           Parser             (parse)
import           System.Environment (getArgs)
import           System.IO          (IOMode (ReadMode), hClose, hGetContents,
                                     openFile)

main :: IO ()
main = do
  args <- getArgs
  if 1 <= length args then do
    handle <- openFile (head args) ReadMode
    contents <- hGetContents handle
    putStrLn $ parse contents
    hClose handle
  else
    putStrLn "Argument not specified"
