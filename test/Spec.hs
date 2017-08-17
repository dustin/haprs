import APRS.Types

import Test.HUnit
import Test.QuickCheck

testCallPass :: Test
testCallPass = TestList (
  map (\(s, want) -> TestCase (assertEqual s (callPass $ read s) want)) [
      ("KG6HWF-9", 22955),
      ("KG6HWF", 22955),
      ("KE6AFE-13", 18595)])

testAddressParsing :: Test
testAddressParsing = TestList (
  map (\(s, want) -> TestCase (assertEqual s (read s) want)) [
      ("KG6HWF-11", Address "KG6HWF" "11"),
      ("KG6HWF", Address "KG6HWF" ""),
      ("KG6HWF-9", Address "KG6HWF" "9")])

tests :: Test
tests = TestList [
  TestLabel "callpass" testCallPass,
  TestLabel "address" testAddressParsing
  ]

main :: IO Counts
main = do runTestTT tests


