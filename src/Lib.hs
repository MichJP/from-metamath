module Lib
    ( compile
    ) where

import Data.List

compile :: String -> String
compile contents = stripComments contents

stripComments :: String -> String
stripComments "" = ""
stripComments ('$':'(':xs) = stripComments' xs
stripComments (x:xs) = x : stripComments xs

stripComments' :: String -> String
stripComments' "" = error "No matching $) found!"
stripComments' ('$':')':xs) = stripComments xs
stripComments' (_:xs) = stripComments' xs

