module Lib
    ( someFunc
    ) where

import System.IO
import Data.List

someFunc :: IO ()
someFunc = do
  handle <- openFile "test/tutorial_example.mm" ReadMode 
  contents <- hGetContents handle
  putStrLn . stripComments $ contents
  let tokens = collectMetaTokens contents
  putStrLn . intercalate ", " $ tokens
  putStrLn . show $ length tokens
  let sortedTokens = sort tokens
  let tokenGroups = group sortedTokens
  let tokenCount = map length tokenGroups
  let tokenLabels = map head tokenGroups
  putStrLn . show $ zip tokenLabels tokenCount
  hClose handle

stripComments :: String -> String
stripComments "" = ""
stripComments ('$':'(':xs) = stripComments' xs
stripComments (x:xs) = x : stripComments xs

stripComments' :: String -> String
stripComments' "" = error "No matching $) found!"
stripComments' ('$':')':xs) = stripComments xs
stripComments' (_:xs) = stripComments' xs

collectMetaTokens :: String -> [String]
collectMetaTokens "" = []
collectMetaTokens ('$':x:xs) = ('$':x:""):collectMetaTokens xs
collectMetaTokens (_:xs) = collectMetaTokens xs

