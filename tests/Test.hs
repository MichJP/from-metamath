import Control.Monad
import Test.HUnit

import Lib

main :: IO ()
main = void (runTestTT (TestList tests))

mkTest label expr expected =
  TestLabel label $ TestCase (assertEqual ""  expected (compile expr))

test1 = mkTest "comment1" "$( comment $)" (Right True)
test2 = mkTest "comment2" "$( nested $( comment $) $)" (Right False)
test3 = mkTest "cap1" "$c f x $. t $a f x $. $p f x $= t $." (Right True)

tests =
  [
    test1
  , test2
  , test3
  ]

