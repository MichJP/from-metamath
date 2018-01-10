import Control.Monad
import System.IO
import Test.HUnit

import Lib

main :: IO ()
main = void (runTestTT (TestList tests))

mkTest label expr expected =
  TestLabel label $ expected ~=? compile expr

test1 = mkTest "comment1" "$( comment $)" (Right True)
test2 = TestLabel "comment2" $ TestCase (case compile "$( nested $( comment $) $)" of
                             Left _ -> return ()
                             Right _ -> assertFailure "Nested comments parsed successfully!")
test3 = mkTest "cap1" "$c f x $. t $a f x $. $p f x $= t $." (Right True)
test4 = mkTest "c1" "$c f x $." (Right True)
test5 = mkTest "c2" "$c $. $." (Right False)

tests =
  [
    test1
  , test2
  , test3
  , test4
  , test5
  ]

