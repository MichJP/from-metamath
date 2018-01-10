module Lib where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
isValid = isValid' [] Map.empty

isValid' :: [Symbol] -> Map Label [Symbol] -> [Statement] -> Bool
isValid' _ _ [] = True
isValid' symbols hypotheses ((Constants cs):xs) =
    isValid' (cs ++ symbols) hypotheses xs
isValid' symbols hypotheses ((Variables vs):xs) =
    error "Not implemented yet."
isValid' symbols hypotheses ((Axiom lb sbs):xs) = and
    [ all (`elem` symbols) sbs
    , isValid' symbols (Map.insert lb sbs hypotheses) xs
    ]
isValid' symbols hypotheses ((Theorem lb sbs proof):xs) = and
    [ all (`elem` symbols) sbs
    , all (`Map.member` hypotheses) proof
    , proof `reduceWith` hypotheses == sbs
    , isValid' symbols (Map.insert lb sbs hypotheses) xs
    ]

reduceWith :: [Label] -> Map Label [Symbol] -> [Symbol]
reduceWith [] _ = []
reduceWith (lb:lbs) hypotheses = Map.findWithDefault [] lb hypotheses ++ reduceWith lbs hypotheses


