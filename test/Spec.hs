import APRS.Types

import Test.HUnit
import Test.QuickCheck

testCallPass :: Test
testCallPass = TestCase (assertEqual "callpass" 22955 (callPass $ Address {call = "KG6HWF", ssid = "11"}))

tests :: Test
tests = TestList [TestLabel "callpass" testCallPass]

main :: IO Counts
main = do runTestTT tests

