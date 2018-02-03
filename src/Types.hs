module Types where

-- identifier for $f, $e, $a, and $p statements
type Label = String

-- math symbols declared as constants or variables
type Symbol = String

-- ${, $}, $c, $v, $f, $e, $d, $a, or $p statement
data Statement =
    Constants [Symbol]
  | Variables [Symbol]
  | Axiom Label [Symbol]
  | Theorem Label [Symbol] [Label]
  deriving (Show)

-- all keywords except comment $( $) and file inclusion $[ $]
data Keyword =
    CONSTANTS -- $c
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

-- string representations of keyword tokens
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

-- list of keyword tokens as strings
allKeywords :: [String]
allKeywords =
  map show [ CONSTANTS
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

