{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-type-defaults #-}

module APRSTests (tests) where

import APRS.Types

import Data.String (fromString)
import Data.Either (isRight)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit


addrChars :: [Char]
addrChars = ['A'..'Z'] ++ ['0'..'9']

instance Arbitrary Address where
  arbitrary = do
    l <- choose (3, 12)
    r <- choose (0, 5)
    lel <- shuffle addrChars
    rel <- shuffle addrChars
    return $ must $ address ((fromString.take l) lel) ((fromString.take r) rel)

testCallPass :: [TestTree]
testCallPass =
  map (\(s, want) -> testCase s $ assertEqual s (callPass $ read s) want) [
    ("KG6HWF-9", 22955),
    ("KG6HWF", 22955),
    ("KE6AFE-13", 18595),
    ("K6MGD", 12691)]

testAddressParsing :: [TestTree]
testAddressParsing =
  map (\(s, want) -> testCase s $ assertEqual s (read s) want) [
    ("KG6HWF-11", must $ address "KG6HWF" "11"),
    ("KG6HWF", must $ address "KG6HWF" ""),
    ("KG6HWF-9", must $ address "KG6HWF" "9")]

raddr :: String -> Address
raddr = read

testAddrSimilar :: [TestTree]
testAddrSimilar =
  map (\(a, b) -> testCase (a ++ " ≈ " ++ b) $ assertBool "" (raddr a ≈ raddr b)) [
  ("KG6HWF", "KG6HWF"),
  ("KG6HWF-9", "KG6HWF"),
  ("KG6HWF", "KG6HWF-11"),
  ("KG6HWF-9", "KG6HWF-11")]

testBase91 :: [TestTree]
testBase91 =
  map (\(a, want) -> testCase (show a ++ " -> " ++ show want) $ assertEqual "" (decodeBase91 a)    want) [
  (['\0', '\0', '\0', '\0'], -25144152),
  (['\1', '\0', '\0', '\0'], -24390581),
  (['\1', '\0', '\0', '\1'], -24390580),
  (['\1', '\0', '\xff', '\1'], -24367375),
  ("", 0),
  ("a", 0),
  ("ab", 0),
  ("abc", 0),
  ("abcde", 0),
  ("<*e7", 20346417 + 74529 + 6188 + 22)]

prop_roundtrips :: (Show a, Read a, Eq a) => a -> Bool
prop_roundtrips x = read (show x) == x

christmasMsg :: String
christmasMsg = "KG6HWF>APX200,WIDE1-1,WIDE2-1:=3722.1 N/12159.1 W-Merry Christmas!"

instance Arbitrary Frame where
  arbitrary = do
    src <- arbitrary ::Gen Address
    dst <- arbitrary ::Gen Address
    msg <- arbitrary ::Gen String
    return Frame { source = src,
                   dest = dst,
                   APRS.Types.path = ["WIDE1-1", "WIDE2-1"],
                   body = Body (fromString msg) }

rframe :: String -> Frame
rframe = read

testChristmasMsg :: Assertion
testChristmasMsg =
  assertEqual "christmas parsing" (raddr "KG6HWF") $ source $ rframe christmasMsg

propValidAddress :: String -> String -> Property
propValidAddress a s
  | null a = collect "short" $ addr == Left "callsign is too short"
  | length a > 12 = collect "long" $ addr == Left "callsign is too long"
  | length s > 6 = collect "long ssid" $  addr == Left "SSID is too long"
  | any (`notElem` addrChars) a = collect "call invalid" $ addr == Left "invalid characters in callsign"
  | any (`notElem` addrChars) s = collect "SSID invalid" $ addr == Left "invalid characters in SSID"
  | otherwise = collect "other" $ isRight addr
  where addr = address (fromString a) (fromString s)

propSplitOnSplits :: NonEmptyList Char -> NonEmptyList Char -> Property
propSplitOnSplits (NonEmpty a) (NonEmpty b) = nospace ==> splitOn ' ' (a ++ " " ++ b) == (a, b)
  where nospace = nospace' a && nospace' b
        nospace' l = ' ' `notElem` l

propSplitOnMultiSplits :: NonEmptyList Char -> NonEmptyList Char -> Property
propSplitOnMultiSplits (NonEmpty a) (NonEmpty b) = nospace ==> splitOn ' ' (a ++ "  " ++ b) == (a, " " ++ b)
  where nospace = nospace' a && nospace' b
        nospace' l = ' ' `notElem` l

tests :: [TestTree]
tests = [
  testGroup "callPass"  testCallPass,
  testGroup "addrParse" testAddressParsing,
  testProperty "address round trips" (prop_roundtrips :: Address -> Bool),
  testGroup "addrSimilar" testAddrSimilar,
  testCase "frame parsing" testChristmasMsg,
  testProperty "frame round trips" (prop_roundtrips :: Frame -> Bool),
  testProperty "address validation" propValidAddress,
  testGroup "base91" testBase91,

  testProperty "split splits on" propSplitOnSplits,
  testProperty "split splits on multi" propSplitOnMultiSplits
  ]
