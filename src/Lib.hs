module Lib
    ( someFunc
    ) where

import System.IO

someFunc :: IO ()
someFunc = do
  handle <- openFile "test/tutorial_example.mm" ReadMode 
  contents <- hGetContents handle
  putStrLn . stripComments $ contents
  hClose handle

stripComments :: String -> String
stripComments "" = ""
stripComments ('$':'(':xs) = stripComments' xs
stripComments (x:xs) = x : stripComments xs

stripComments' :: String -> String
stripComments' "" = error "No matching $) found!"
stripComments' ('$':')': xs) = stripComments xs
stripComments' (_:xs) = stripComments' xs

