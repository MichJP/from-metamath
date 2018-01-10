import Control.Monad
import System.IO
import Test.HUnit

import Lib

main :: IO ()
main = void (runTestTT (TestList tests))

mkTest label expr expected =
  TestLabel label $ expected ~=? compile expr

test1 = mkTest "comment1" "$( comment $)" (Right True)
test2 = TestLabel "comment2" $ TestCase res
  where res = case compile "$( nested $( comment $) $)" of
                Left _ -> return ()
                Right _ -> assertFailure "Nested comments parsed successfully!"
test3 = mkTest "cap1" "$c f x $. t $a f x $. k $p f x $= t $." (Right True)
test4 = mkTest "c1" "$c f x $." (Right True)
test5 = TestLabel "c2" $ TestCase res
  where res = case compile "$c $. $." of
                Left _ -> return ()
                Right _ -> assertFailure "Unpaired $. parsed successfully!"
test6 = mkTest "ca1" "$c f x $. t $a f x $." (Right True)
test7 = mkTest "ca2" "$c f x $. t $a f x $. u $a f x $." (Right True)

tests =
  [
    test1
  , test2
  , test3
  , test4
  , test5
  ]

