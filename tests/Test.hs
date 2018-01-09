import Control.Monad
import Test.HUnit

import Lib

main :: IO ()
main = void (runTestTT (TestList tests))

mkTest expr expected = TestCase (assertEqual ""  expected (compile expr))

test1 = TestLabel "comment1" $ mkTest "$( comment $)" ""

tests = [ test1 ]

