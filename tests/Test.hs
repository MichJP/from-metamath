import Control.Monad
import System.IO
import Test.HUnit

import Lib

main :: IO ()
main = void (runTestTT (TestList tests))

mkTest label expr expected =
  TestLabel label $ expected ~=? compile expr

mkTest2 label expr msg = TestLabel label $ TestCase res
  where res = case compile expr of
                Left _ -> return ()
                Right _ -> assertFailure msg

test1 = mkTest "comment1"
               "$( comment $)"
               (Right True)
test2 = mkTest2 "comment2"
                "$( nested $( comment $) $)"
                "Nested comments parsed successfully!"
test3 = mkTest "cap1"
               "$c f x $. t $a f x $. k $p f x $= t $."
               (Right True)
test4 = mkTest "c1"
               "$c f x $."
               (Right True)
test5 = mkTest2 "c2"
                "$c $. $."
                "Unpaired $. parsed successfully!"
test6 = mkTest "ca1"
               "$c f x $. t $a f x $."
               (Right True)
test7 = mkTest "ca2"
               "$c f x $. t $a f x $. u $a f x $."
               (Right True)
test8 = mkTest2 "ca3"
                "$cxyz f x $. t $a f x $. u $a f x $."
                "Invalid keyword sequence $cxyz parsed successfully!"
test9 = mkTest "incorrect-proof-1"
               "$c x y $. t $a y x $. u $p x y $= t $."
               (Right False)

tests =
  [
    test9
  , test8
  , test7
  , test6
  , test5
  , test4
  , test3
  , test2
  , test1
  ]

