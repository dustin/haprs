import APRS.Types

import Test.HUnit (Assertion, assertEqual, assertBool)
import Test.QuickCheck
import Test.Framework.Runners.Options
import Test.Framework.Options (TestOptions'(..))
import Test.Framework (defaultMainWithOpts, interpretArgsOrExit, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import System.Environment (getArgs)

addrChars :: [Char]
addrChars = ['A'..'Z'] ++ ['0'..'9']

instance Arbitrary Address where
  arbitrary = do
    l <- choose (3, 12)
    r <- choose (0, 5)
    lel <- shuffle addrChars
    rel <- shuffle addrChars
    return $ must $ address (take l lel) (take r rel)

testCallPass :: [Test]
testCallPass =
  map (\(s, want) -> testCase s $ assertEqual s (callPass $ read s) want) [
    ("KG6HWF-9", 22955),
    ("KG6HWF", 22955),
    ("KE6AFE-13", 18595),
    ("K6MGD", 12691)]

testAddressParsing :: [Test]
testAddressParsing =
  map (\(s, want) -> testCase s $ assertEqual s (read s) want) [
    ("KG6HWF-11", must $ address "KG6HWF" "11"),
    ("KG6HWF", must $ address "KG6HWF" ""),
    ("KG6HWF-9", must $ address "KG6HWF" "9")]

raddr :: String -> Address
raddr a = read a

testAddrSimilar :: [Test]
testAddrSimilar =
  map (\(a, b) -> testCase (a ++ " ≈ " ++ b) $ assertBool "" (raddr a ≈ raddr b)) [
  ("KG6HWF", "KG6HWF"),
  ("KG6HWF-9", "KG6HWF"),
  ("KG6HWF", "KG6HWF-11"),
  ("KG6HWF-9", "KG6HWF-11")]

testBase91 :: [Test]
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
                   body = Body msg }

rframe :: String -> Frame
rframe a = read a

testChristmasMsg :: Assertion
testChristmasMsg =
  assertEqual "christmas parsing" (raddr "KG6HWF") $ source $ rframe christmasMsg

propValidAddress :: String -> String -> Property
propValidAddress a s
  | a == [] = collect "short" $ not valid
  | length a > 12 = collect "long" $ not valid
  | length s > 6 = collect "long ssid" $ not valid
  | all (`elem` addrChars) (a++s) = collect "valid" $ valid
  | otherwise = collect "other" $ not valid
  where valid = case address a s of
                  Left _ -> False
                  Right _ -> True

tests :: [Test]
tests = [
  testGroup "callPass"  testCallPass,
  testGroup "addrParse" testAddressParsing,
  testProperty "address round trips" (prop_roundtrips :: Address -> Bool),
  testGroup "addrSimilar" testAddrSimilar,
  testCase "frame parsing" testChristmasMsg,
  testProperty "frame round trips" (prop_roundtrips :: Frame -> Bool),
  testProperty "address validation" propValidAddress,
  testGroup "base91" testBase91
  ]

main :: IO ()
main = do opts <- interpretArgsOrExit =<< getArgs
          defaultMainWithOpts tests
            opts { ropt_hide_successes = Just True,
                   ropt_test_options = Just $ mempty { topt_maximum_generated_tests = Just 500 }}
