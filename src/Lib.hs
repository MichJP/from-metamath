module Lib
    where

import Control.Applicative hiding (Const)
import Control.Monad (void)
import Data.List
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

type Label = String

data Symbol = Const String
            | Var String

data Statement = Constants [Symbol]
               | Variables [Symbol]
               | Axiom Label [Symbol]
               | Theorem Label [Symbol] [Label]

data Keyword = CONSTANTS -- $c
             | VARIABLES -- $v
             | FLOATING_HYPOTHESIS -- $f
             | ESSENTIAL_HYPOTHESIS -- $e
             | DISJOINT_VARIABLE_RESTRICTION -- $d
             | AXIOMATIC_ASSERTION -- $a
             | PROVABLE_ASSERTION -- $p
             | STATEMENT_TERMINATOR -- $.
             | PROOF -- $=
             | BEGIN -- ${
             | END -- $}

instance Show Keyword where
  show CONSTANTS = "$c"
  show VARIABLES = "$v"
  show FLOATING_HYPOTHESIS = "$f"
  show ESSENTIAL_HYPOTHESIS = "$e"
  show DISJOINT_VARIABLE_RESTRICTION = "$d"
  show AXIOMATIC_ASSERTION = "$a"
  show PROVABLE_ASSERTION = "$p"
  show STATEMENT_TERMINATOR = "$."
  show PROOF = "$="
  show BEGIN = "${"
  show END = "$}"

keywords :: [String]
keywords = map show [ CONSTANTS, VARIABLES, FLOATING_HYPOTHESIS, ESSENTIAL_HYPOTHESIS, DISJOINT_VARIABLE_RESTRICTION, AXIOMATIC_ASSERTION, PROVABLE_ASSERTION, STATEMENT_TERMINATOR, PROOF, BEGIN, END ]

compile :: String -> Either String Bool
compile input = case parse statementList "(stdin)" input of
                  Right statements -> Right (isValid statements)
                  Left err -> Left $ parseErrorPretty err

isValid :: [Statement] -> Bool
isValid [] = True
isValid _ = False

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt = empty
        blockCmnt = L.skipBlockComment "$(" "$)"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

syntaxElement :: (String -> a) -> Parser a
syntaxElement f = (lexeme . try) (some (alphaNumChar <|> punctuationChar) >>= check)
  where check x = if x `elem` keywords
                    then fail $ "keyword " ++ show x ++ " cannot be a symbol"
                    else return $ f x

syntaxElementList f = many $ syntaxElement f

symbol :: String -> Parser String
symbol = L.symbol sc

statementTerminator = symbol $ show STATEMENT_TERMINATOR

declaration kwd@CONSTANTS =
    between (symbol $ show kwd)
            statementTerminator
            (syntaxElementList Const)
declaration kwd@VARIABLES =
    between (symbol $ show kwd)
            statementTerminator
            (syntaxElementList Var)
declaration kwd = fail $ "keyword " ++ show kwd ++ " cannot be declared"

constants :: Parser Statement
constants = Constants <$> declaration CONSTANTS

variables :: Parser Statement
variables = Variables <$> declaration VARIABLES

statementLabel = syntaxElement id

statementLabelList = many statementLabel

axiomaticAssertion = do
  assertionLabel <- statementLabel
  symbols <- between (symbol $ show AXIOMATIC_ASSERTION)
                     statementTerminator
                     (syntaxElementList Const)
  return $ Axiom assertionLabel symbols

provableAssertion = do
  assertionLabel <- statementLabel
  symbol $ show PROVABLE_ASSERTION
  theorem <- syntaxElementList Const
  symbol $ show PROOF
  proof <- statementLabelList
  statementTerminator
  return $ Theorem assertionLabel theorem proof

statement =   constants
          <|> axiomaticAssertion
          <|> provableAssertion

statementList = sc *> many statement <* eof

