module ParserTest
  ( parserTests
  ) where

import Test.HUnit

import Parser
import Types

mkTest label expr expected =
  TestLabel label $ expected ~=? expr

test1 = mkTest "keyword-1-const" (show CONSTANTS) "$c"
test2 = mkTest "keyword-2-var" (show VARIABLES) "$v"
test3 = mkTest "keyword-3-fh" (show FLOATING_HYPOTHESIS) "$f"
test4 = mkTest "keyword-4-eh" (show ESSENTIAL_HYPOTHESIS) "$e"
test5 = mkTest "keyword-5-dvr" (show DISJOINT_VARIABLE_RESTRICTION) "$d"
test6 = mkTest "keyword-6-aa" (show AXIOMATIC_ASSERTION) "$a"
test7 = mkTest "keyword-7-pa" (show PROVABLE_ASSERTION) "$p"
test8 = mkTest "keyword-8-st" (show STATEMENT_TERMINATOR) "$."
test9 = mkTest "keyword-9-proof" (show PROOF) "$="
test10 = mkTest "keyword-10-begin" (show BEGIN) "${"
test11 = mkTest "keyword-11-end" (show END) "$}"

parserTests = [
    test11
  , test10
  , test9
  , test8
  , test7
  , test6
  , test5
  , test4
  , test3
  , test2
  , test1
  ]

