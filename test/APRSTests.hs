{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-type-defaults #-}

module APRSTests (tests) where

import APRS.Types

import Data.Char (chr)
import Data.Either (isRight, either)
import Data.String (fromString)
import Data.Word (Word8)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import qualified Data.Attoparsec.Text as A
import qualified Data.Set as Set
import qualified Data.Text as T


addrChars :: [Char]
addrChars = ['A'..'Z'] ++ ['0'..'9']

arbitraryText :: String -> (Int, Int) -> Gen T.Text
arbitraryText chars range = do
    l <- choose range
    a <- vectorOf l $ elements chars
    return (fromString $ take l a)

must :: Either String a -> a
must = either undefined id

newtype ArbitraryCall = ArbitraryCall T.Text deriving Show

instance Arbitrary ArbitraryCall where
  arbitrary = pure.ArbitraryCall =<< arbitraryText addrChars (1, 12)

newtype ArbitrarySSID = ArbitrarySSID T.Text deriving Show

instance Arbitrary ArbitrarySSID where
  arbitrary = pure.ArbitrarySSID =<< arbitraryText addrChars (0, 6)

instance Arbitrary Address where
  arbitrary = do
    (ArbitraryCall c) <- arbitrary
    (ArbitrarySSID s) <- arbitrary
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
    return $ Frame src dst ["WIDE1-1", "WIDE2-1"] (Body (fromString msg))

rframe :: String -> Frame
rframe = read

testChristmasMsg :: Assertion
testChristmasMsg =
  assertEqual "christmas parsing" (raddr "KG6HWF") $ source $ rframe christmasMsg
  where source (Frame s _ _ _) = s

propValidAddress :: String -> String -> Property
propValidAddress a s
  | null a = collect "short" $ addr == Left "callsign is too short"
  | length a > 12 = collect "long" $ addr == Left "callsign is too long"
  | length s > 6 = collect "long ssid" $  addr == Left "SSID is too long"
  | any (`notElem` addrChars) a = collect "call invalid" $ addr == Left "invalid characters in callsign"
  | any (`notElem` addrChars) s = collect "SSID invalid" $ addr == Left "invalid characters in SSID"
  | otherwise = collect "other" $ isRight addr
  where addr = address (fromString a) (fromString s)

testVelocityPrinting :: Assertion
testVelocityPrinting = assertEqual "vel" "23.0 kph @273°" (show $ Velocity (273, 23))

testNoDupMapping :: (Bounded a, Enum a, Show b, Ord b) => (a -> b) -> Assertion
testNoDupMapping f = case foldr findDup (Right Set.empty) [minBound..] of
                       Left x -> assertString $ "Duplicate value found: " ++ show x
                       Right _ -> return ()
  where findDup x s = let p = f x in case Set.member p <$> s of
                                       Right True -> Left p
                                       _ -> Set.insert p <$> s

testTimestampParser :: [TestTree]
testTimestampParser =
  map (\(a, want) -> testCase (show a ++ " -> " ++ show want) $ assertEqual "" (A.parseOnly parseTimestamp a) want) [
  ("092345z", Right $ DHMZulu (9, 23, 45)),
  ("092345/", Right $ DHMLocal (9, 23, 45)),
  ("234517h", Right $ HMS (23, 45, 17)),
  ("10092345", Right $ MDHM (10, 9, 23, 45))
  ]

  -- EIWX11>APRS,qAS,WXSVR-EI:@181800z5147.88N/00815.00W_180/006g...t054r000p...P...b10140h88ROCHES POINT, RECENT DRIZZLE [http://www.met.ie]


testWeatherParser :: [TestTree]
testWeatherParser =
  map (\(a, want) -> testCase (show a) $ assertEqual "" want (A.parseOnly parseWeather a)) [
  ("g005t077r000p000P000h50b09900wRSW", Right [WindGust 5,Temp 77,RainLastHour 0,
                                               RainLast24Hours 0,RainToday 0,Humidity 50,Baro 9900]),
  ("c220s004g005t077r000p000P000h50b09900wRSW", Right [WindDir 220,WindSpeed 4,WindGust 5,Temp 77,
                                                       RainLastHour 0,RainLast24Hours 0,RainToday 0,
                                                       Humidity 50,Baro 9900]),
  ("g...t054r000p...P...b10140h88", Right [NoData 'g', Temp 54, RainLastHour 0, NoData 'p', NoData 'P',
                                           Baro 10140, Humidity 88])
  ]

testMegaParser :: [TestTree]
testMegaParser =
  map (\(a, want) -> testCase (show a) $ assertEqual "" want (A.parseOnly megaParser a)) [
  ("!4903.50N/07201.75W-Test 001234",
   Right (PositionPacket PositionNoTSNoMsg (Symbol '/' '-')
          (49.05833333333333,-72.02916666666667) Nothing PosENone "Test 001234")),
  ("!0000.00N\\00000.00W-Unknown pos",
   Right (PositionPacket PositionNoTSNoMsg (Symbol '\\' '-')
          (0,0) Nothing PosENone "Unknown pos")),
  ("!4903.50N/07201.75W-Test /A=001234", -- TODO:  Parse out the altitude (1234 feet, anywhere in comment)
   Right (PositionPacket PositionNoTSNoMsg (Symbol '/' '-')
          (49.05833333333333,-72.02916666666667) Nothing PosENone "Test /A=001234")),
  ("!49  .  N/072  .  W-",
   Right (PositionPacket PositionNoTSNoMsg (Symbol '/' '-')
          (49.5,-72.5) Nothing PosENone "")),
  ("/092345z4903.50N/07201.75W>Test1234",
   Right (PositionPacket PositionNoMsg (Symbol '/' '>')
          (49.05833333333333,-72.02916666666667) (Just (DHMZulu (9,23,45))) PosENone "Test1234")),
  ("@092345/4903.50N/07201.75W>Test1234",
   Right (PositionPacket PositionMsg (Symbol '/' '>')
          (49.05833333333333,-72.02916666666667) (Just (DHMLocal (9,23,45))) PosENone "Test1234")),
  ("=/5L!!<*e7> sTComment",
   Right (PositionPacket PositionNoTS (Symbol '/' '>')
          (49.5,-72.75000393777269) Nothing PosENone "Comment")),
  ("@092345z/5L!!<*e7>{?! ",
   Right (PositionPacket PositionMsg (Symbol '/' '>')
          (49.5,-72.75000393777269) (Just (DHMZulu (9,23,45))) PosENone " ")),
  (";LEADER   _092345z4903.50N/07201.75W>088/036",
   Right (ObjectPacket (Symbol '/' '>') -- this one eats the 088/036 course/speed
          "LEADER   " (49.05833333333333,-72.02916666666667) (DHMZulu (9,23,45)) "")),
  (";LEADER   *092345z/5L!!<*e7>7P[ ",
   Right (ObjectPacket (Symbol '/' '>')
          "LEADER   " (49.5,-72.75000393777269) (DHMZulu (9,23,45)) " ")),
  (")AID #2!4903.50N/07201.75WA",
    Right (ItemPacket (Symbol '/' 'A') "AID #2" (49.05833333333333,-72.02916666666667) "")),
  (")G/WB4APR!53  .  N\\002  .  Wd", Right (ItemPacket (Symbol '\\' 'd') "G/WB4APR" (53.5,-2.5) "")),
  (")MOBIL!\\5L!!<*e79 sT", Right (ItemPacket (Symbol '\\' '9')
                                    "MOBIL" (49.5,-72.75000393777269) "")),
  ("_10090556c220s004g005t077r000p000P000h50b09900wRSW",
   Right (WeatherPacket (Just (MDHM (10,9,5,56))) Nothing
          [WindDir 220,WindSpeed 4,WindGust 5,Temp 77,RainLastHour 0,
           RainLast24Hours 0,RainToday 0,Humidity 50,Baro 9900] "wRSW")),
  ("!4903.50N/07201.75W_220/004g005t077r000p000P000h50b09900wRSW",
   Right (WeatherPacket Nothing (Just (49.05833333333333,-72.02916666666667,PosECourseSpeed 220 7.408))
          [WindGust 5,Temp 77,RainLastHour 0,RainLast24Hours 0,
           RainToday 0,Humidity 50,Baro 9900] "wRSW")),

  ("T#MIC199,000,255,073,123,01101001",
    Right (TelemetryPacket "MIC" [199,0,255,73,123] 105 "")),
  ("T#MIC,199,000,255,073,123,01101001",
    Right (TelemetryPacket "MIC" [199,0,255,73,123] 105 "")),
  ("T#005,199,000,255,073,123,01101001",
    Right (TelemetryPacket "005" [199,0,255,73,123] 105 "")),
  ("T#005,199,000,255,073,123,01101001 with a comment",
    Right (TelemetryPacket "005" [199,0,255,73,123] 105 " with a comment")),


  -- Some samples from FAP
  ("!4526.66NI01104.68E#PHG21306/- Lnx APRS Srv - sez. ARI VR EST",
   Right (PositionPacket PositionNoTSNoMsg (Symbol 'I' '#')
          (45.44433333333333,11.078) Nothing (PosEPHG 4 20 3 Omni) "6/- Lnx APRS Srv - sez. ARI VR EST")),
  ("/055816h5134.38N/00019.47W>155/023!W26!/A=000188 14.3V 27C HDOP01.0 SATS09",
   Right (PositionPacket PositionNoMsg (Symbol '/' '>')
          (51.573,-0.32449999999999996) (Just (HMS (5,58,16))) (PosECourseSpeed 155 42.596000000000004)
          "!W26!/A=000188 14.3V 27C HDOP01.0 SATS09")),
  (":OH7LZB   :Testing, 1 2 3{1", Right (MessagePacket (raddr "OH7LZB")
                                         (Message' "Testing, 1 2 3") "1")),
  (":OH7LZB   :Testing, 1 2 3", Right (MessagePacket (raddr "OH7LZB")
                                       (Message' "Testing, 1 2 3") "")),
  (":OH7LZB   :ack1", Right (MessagePacket (raddr "OH7LZB") MessageACK "1")),
  (":OH7LZB   :rej1", Right (MessagePacket (raddr "OH7LZB") MessageNAK "1")),

  ("!I0-X;T_Wv&{-Aigate testing",
   Right (PositionPacket PositionNoTSNoMsg (Symbol 'I' '&')
          (60.052010101699544,24.504507437140035) Nothing PosENone "igate testing"))

  ]

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
  testCase "no dup packet types" $ testNoDupMapping (identifyPacket.chr.fromIntegral :: Word8 -> PacketType),

  testCase "velocity prints" testVelocityPrinting,
  testGroup "timestamp parsing" testTimestampParser,
  testGroup "weather parsing" testWeatherParser,
  testGroup "mega parser" testMegaParser
  ]
