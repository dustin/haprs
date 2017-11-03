{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-type-defaults #-}

module APRSTests (tests) where

import APRS.Types

import Data.String (fromString)
import Data.Either (isRight)
import Data.Word (Word8)
import Data.Char (chr)
import qualified Data.Text as T
import qualified Data.Set as Set
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit


addrChars :: [Char]
addrChars = ['A'..'Z'] ++ ['0'..'9']

newtype ArbitraryCall = ArbitraryCall T.Text deriving Show

instance Arbitrary ArbitraryCall where
  arbitrary = do
    l <- choose (1, 12)
    a <- vectorOf l $ elements addrChars
    return $ ArbitraryCall (fromString $ take l a)

newtype ArbitrarySSID = ArbitrarySSID T.Text deriving Show

instance Arbitrary ArbitrarySSID where
  arbitrary = do
    l <- choose (0, 6)
    a <- vectorOf l $ elements addrChars
    return $ ArbitrarySSID (fromString $ take l a)

instance Arbitrary Address where
  arbitrary = do
    (ArbitraryCall c) <- arbitrary :: Gen ArbitraryCall
    (ArbitrarySSID s) <- arbitrary :: Gen ArbitrarySSID
    return $ must $ address c s

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

-- Same as the above, but as a property test.
propAddrSimilar :: ArbitraryCall -> ArbitrarySSID -> ArbitrarySSID -> Bool
propAddrSimilar (ArbitraryCall c) (ArbitrarySSID s1) (ArbitrarySSID s2) =
  ma c s1 ≈ ma c s2
  where ma c' s' = must (address c' s')

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
propSplitOnSplits (NonEmpty a) (NonEmpty b) = nospace ==> splitOn' ' ' (a ++ " " ++ b) == (a, b)
  where nospace = nospace' a && nospace' b
        nospace' l = ' ' `notElem` l

propSplitOnMultiSplits :: NonEmptyList Char -> NonEmptyList Char -> Property
propSplitOnMultiSplits (NonEmpty a) (NonEmpty b) = nospace ==> splitOn' ' ' (a ++ "  " ++ b) == (a, " " ++ b)
  where nospace = nospace' a && nospace' b
        nospace' l = ' ' `notElem` l

testBadPositions :: IO ()
testBadPositions = do
  assertEqual "empty" (position (Body "")) Nothing
  assertEqual "!" (position (Body "!")) Nothing
  assertEqual "x" (position (Body "x")) Nothing
  assertEqual "bad new pu" (position (Body "!12345678")) Nothing
  assertEqual "bad new pc" (position (Body "!x2345678")) Nothing

testNoDupMapping :: (Bounded a, Ord a, Enum a, Show b, Ord b) => (a -> b) -> Assertion
testNoDupMapping f = case foldr findDup (Right Set.empty) [minBound..] of
                       Left x -> assertString $ "Duplicate value found: " ++ show x
                       Right _ -> return ()
  where findDup x s = let p = f x in case Set.member p <$> s of
                                       Right True -> Left p
                                       _ -> Set.insert p <$> s

tests :: [TestTree]
tests = [
  testGroup "callPass"  testCallPass,
  testGroup "addrParse" testAddressParsing,
  testProperty "address round trips" (prop_roundtrips :: Address -> Bool),
  testGroup "addrSimilar" testAddrSimilar,
  testProperty "addrSimilar" propAddrSimilar,
  testCase "frame parsing" testChristmasMsg,
  testProperty "frame round trips" (prop_roundtrips :: Frame -> Bool),
  localOption (QC.QuickCheckTests 1000) $ testProperty "address validation" propValidAddress,
  testGroup "base91" testBase91,
  testCase "bad positions" testBadPositions,
  testCase "no dup packet types" $ testNoDupMapping (identifyPacket.chr.fromIntegral :: Word8 -> PacketType),

  testProperty "split splits on" propSplitOnSplits,
  testProperty "split splits on multi" propSplitOnMultiSplits
  ]
