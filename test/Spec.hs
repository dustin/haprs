import APRS.Types

import Test.HUnit
import Test.QuickCheck
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

testCallPass =
  map (\(s, want) -> testCase s $ assertEqual s (callPass $ read s) want) [
    ("KG6HWF-9", 22955),
    ("KG6HWF", 22955),
    ("KE6AFE-13", 18595),
    ("K6MGD", 12691)]

testAddressParsing =
  map (\(s, want) -> testCase s $ assertEqual s (read s) want) [
    ("KG6HWF-11", address "KG6HWF" "11"),
    ("KG6HWF", address "KG6HWF" ""),
    ("KG6HWF-9", address "KG6HWF" "9")]

prop_roundtrips x = (read (show x)) == x

tests = [
  testGroup "callPass"  testCallPass,
  testGroup "addrParse" testAddressParsing,
  testProperty "round trips" (prop_roundtrips :: Address -> Bool)
  ]

main = defaultMain tests


