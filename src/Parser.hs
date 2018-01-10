module Parser where

import Control.Applicative hiding (Const)
import Control.Monad (void)
import Data.List
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Types

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt = empty
        blockCmnt = L.skipBlockComment "$(" "$)"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

syntaxElement :: (String -> a) -> Parser a
syntaxElement f = (lexeme . try) (some (alphaNumChar <|> punctuationChar) >>= check)
  where check x = if x `elem` allKeywords
                    then fail $ "keyword " ++ show x ++ " cannot be a symbol"
                    else return $ f x

syntaxElementList f = many $ syntaxElement f

keyword :: String -> Parser ()
keyword w = lexeme (string w *> notFollowedBy (alphaNumChar <|> punctuationChar))

statementTerminator = keyword $ show STATEMENT_TERMINATOR

declaration kwd@CONSTANTS =
    between (keyword $ show kwd)
            statementTerminator
            (syntaxElementList Const)
declaration kwd@VARIABLES =
    between (keyword $ show kwd)
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
  symbols <- between (keyword $ show AXIOMATIC_ASSERTION)
                     statementTerminator
                     (syntaxElementList Const) -- TODO: Consider variables
  return $ Axiom assertionLabel symbols

provableAssertion = do
  assertionLabel <- statementLabel
  keyword $ show PROVABLE_ASSERTION
  theorem <- syntaxElementList Const -- TODO: Consider variables
  keyword $ show PROOF
  proof <- statementLabelList
  statementTerminator
  return $ Theorem assertionLabel theorem proof

statement =   constants
          <|> try axiomaticAssertion
          <|> provableAssertion

statementList = sc *> many statement <* eof
