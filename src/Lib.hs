module Lib
    ( compile
    ) where

import Data.List

compile :: String -> String
compile contents =
    unlines [
        src
      , intercalate ", " tokens
      , show $ length tokens
      , show $ zip tokenLabels tokenCount
      , show constantTokens
      , show $ length constantTokens
      , show variables
    ]
  where src = stripComments contents
        tokens = collectMetaTokens contents
        sortedTokens = sort tokens
        tokenGroups = group sortedTokens
        tokenCount = map length tokenGroups
        tokenLabels = map head tokenGroups
        regularTokens = collectTokens src
        constantTokens = getConstantTokens regularTokens
        variables = getVariables regularTokens

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

collectTokens :: String -> [String]
collectTokens = words

getConstantTokens :: [String] -> [String]
getConstantTokens [] = []
getConstantTokens (('$':'c':""):xs) = getConstantTokens' xs
getConstantTokens (_:xs) = getConstantTokens xs

getConstantTokens' :: [String] -> [String]
getConstantTokens' [] = error "No matching $. found!"
getConstantTokens' (('$':'.':""):xs) = getConstantTokens xs
getConstantTokens' (x:xs) = x : getConstantTokens' xs

getVariables :: [String] -> [String]
getVariables [] = []
getVariables (('$':'v':""):xs) = getVariables' xs
getVariables (_:xs) = getVariables xs

getVariables' :: [String] -> [String]
getVariables' [] = error "No matching $. found!"
getVariables' (('$':'.':""):xs) = getVariables xs
getVariables' (x:xs) = x : getVariables' xs

