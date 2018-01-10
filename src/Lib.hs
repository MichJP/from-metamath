module Lib where

import Text.Megaparsec

import Parser
import Types

compile :: String -> Either String Bool
compile input = case parse statementList "(stdin)" input of
                  Right statements -> Right (isValid statements)
                  Left err -> Left $ parseErrorPretty err

-- A list of statement is valid given the following:
-- 1. Symbols between $a and $. and between $p and $. have
--    occurred previously between $c and $..
-- 2. Labels between $= and $. have occurred previously
--    before $a or $p.
-- TODO: Assert uniqueness of each label.
isValid :: [Statement] -> Bool
isValid = isValid' [] []

isValid' :: [Symbol] -> [Label] -> [Statement] -> Bool
isValid' [] _ _ = True
isValid' symbols labels ((Constants cs):xs) = isValid' (cs ++ symbols) labels xs 
isValid' symbols labels ((Variables vs):xs) = error "Not implemented yet."
isValid' symbols labels ((Axiom lb sbs):xs) = all (`elem` symbols) sbs && isValid' symbols (lb : labels) xs
isValid' symbols labels ((Theorem lb sbs lbs):xs) = all (`elem` symbols) sbs && all (`elem` labels) lbs && isValid' symbols (lb : labels) xs
