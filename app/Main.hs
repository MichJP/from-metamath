module Main where

import Lib
import System.IO

main :: IO ()
main = do
  handle <- openFile "tests/tutorial_example.mm" ReadMode 
  contents <- hGetContents handle
  putStrLn . show $ compile contents
  hClose handle

