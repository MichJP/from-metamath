module Types where

type Label = String

data Symbol = Const String
            | Var String
            deriving (Eq, Show)

data Statement = Constants [Symbol]
               | Variables [Symbol]
               | Axiom Label [Symbol]
               | Theorem Label [Symbol] [Label]
               deriving (Show)

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

allKeywords :: [String]
allKeywords = map show [ CONSTANTS
                       , VARIABLES
                       , FLOATING_HYPOTHESIS
                       , ESSENTIAL_HYPOTHESIS
                       , DISJOINT_VARIABLE_RESTRICTION
                       , AXIOMATIC_ASSERTION
                       , PROVABLE_ASSERTION
                       , STATEMENT_TERMINATOR
                       , PROOF
                       , BEGIN
                       , END
                       ]