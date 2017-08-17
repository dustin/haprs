import APRS.Types

import Test.HUnit
import Test.QuickCheck

testCallPass :: Test
testCallPass = TestCase (assertEqual "callpass" 22955 (callPass $ Address {call = "KG6HWF", ssid = "11"}))


testAddressParsing :: Test
testAddressParsing = TestList (
  map (\(s, want) -> TestCase (assertEqual s (read s) want)) [
      ("KG6HWF-11", Address "KG6HWF" "11"),
      ("KG6HWF", Address "KG6HWF" "")])

tests :: Test
tests = TestList [
  TestLabel "callpass" testCallPass,
  TestLabel "address" testAddressParsing
  ]

main :: IO Counts
main = do runTestTT tests


