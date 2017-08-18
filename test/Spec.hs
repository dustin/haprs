import APRS.Types

import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

addrChars = ['A'..'Z'] ++ ['0'..'9']

instance Arbitrary Address where
  arbitrary = do
    l <- choose (3, 12)
    r <- choose (0, 5)
    lel <- shuffle addrChars
    rel <- shuffle addrChars
    return $ address (take l lel) (take r rel)

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

raddr a = (read a) :: Address

testAddrSimilar =
  map (\(a, b) -> testCase (a ++ " ~= " ++ b) $ assertBool "" ((raddr a) ≈ (raddr b))) [
  ("KG6HWF", "KG6HWF"),
  ("KG6HWF-9", "KG6HWF"),
  ("KG6HWF", "KG6HWF-11"),
  ("KG6HWF-9", "KG6HWF-11")]

prop_roundtrips x = (read $ show x) == x

christmasMsg = "KG6HWF>APX200,WIDE1-1,WIDE2-1:=3722.1 N/12159.1 W-Merry Christmas!"

instance Arbitrary Frame where
  arbitrary = do
    src <- (arbitrary ::Gen Address)
    dst <- (arbitrary ::Gen Address)
    return $ Frame { source = src,
                     dest = dst,
                     APRS.Types.path = ["WIDE1-1", "WIDE2-1"],
                     body = "hi" }

rframe a = (read a) :: Frame

testChristmasMsg =
  assertEqual "christmas parsing" (raddr "KG6HWF") $ source $ rframe christmasMsg

tests = [
  testGroup "callPass"  testCallPass,
  testGroup "addrParse" testAddressParsing,
  testProperty "address round trips" (prop_roundtrips :: Address -> Bool),
  testGroup "addrSimilar" testAddrSimilar,
  testCase "frame parsing" testChristmasMsg,
  testProperty "frame round trips" (prop_roundtrips :: Frame -> Bool)
  ]

main = defaultMain tests


