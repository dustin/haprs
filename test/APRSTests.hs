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
  arbitrary = ArbitraryCall <$> arbitraryText addrChars (1, 12)

newtype ArbitrarySSID = ArbitrarySSID T.Text deriving Show

instance Arbitrary ArbitrarySSID where
  arbitrary = ArbitrarySSID <$> arbitraryText addrChars (0, 6)

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

rframe :: String -> Frame
rframe = must . A.parseOnly parseFrame . fromString

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
  ("g005t077r000p000P000h50b09900wRSW", Right [WindGust 5,Temp 25, RainLastHour 0,
                                               RainLast24Hours 0, RainToday 0, Humidity 50, Baro 9900]),
  ("c220s004g005t077r000p000P000h50b09900wRSW", Right [WindDir 220, WindSpeed 4, WindGust 5, Temp 25,
                                                       RainLastHour 0, RainLast24Hours 0, RainToday 0,
                                                       Humidity 50, Baro 9900]),
  ("g...t054r000p...P...b10140h88", Right [NoData 'g', Temp 12.222222222222221, RainLastHour 0,
                                           NoData 'p', NoData 'P', Baro 10140, Humidity 88]),
  ("g   t054r000p   P   b10140h88", Right [NoData 'g', Temp 12.222222222222221, RainLastHour 0,
                                           NoData 'p', NoData 'P', Baro 10140, Humidity 88])
  ]

testMegaParser :: [TestTree]
testMegaParser =
  map (\(a, want) -> testCase (show a) $ assertEqual "" want (A.parseOnly (bodyParser $ raddr "S32UVT") a)) [
  ("!4903.50N/07201.75W-Test 001234",
   Right (PositionPacket PositionNoTSNoMsg (Symbol '/' '-')
          (Position (49.05833333333333,-72.02916666666667,PosENone)) Nothing "Test 001234")),
  ("!0000.00N\\00000.00W-Unknown pos",
   Right (PositionPacket PositionNoTSNoMsg (Symbol '\\' '-')
          (Position (0,0,PosENone)) Nothing "Unknown pos")),
  ("!4903.50N/07201.75W-Test /A=001234", -- TODO:  Parse out the altitude (1234 feet, anywhere in comment)
   Right (PositionPacket PositionNoTSNoMsg (Symbol '/' '-')
          (Position (49.05833333333333,-72.02916666666667,PosENone)) Nothing "Test /A=001234")),
  ("!49  .  N/072  .  W-",
   Right (PositionPacket PositionNoTSNoMsg (Symbol '/' '-')
          (Position (49.5,-72.5,PosENone)) Nothing "")),
  ("/092345z4903.50N/07201.75W>Test1234",
   Right (PositionPacket PositionNoMsg (Symbol '/' '>')
          (Position (49.05833333333333,-72.02916666666667,PosENone)) (Just (DHMZulu (9,23,45))) "Test1234")),
  ("@092345/4903.50N/07201.75W>Test1234",
   Right (PositionPacket PositionMsg (Symbol '/' '>')
          (Position (49.05833333333333,-72.02916666666667,PosENone)) (Just (DHMLocal (9,23,45))) "Test1234")),
  ("=/5L!!<*e7> sTComment",
   Right (PositionPacket PositionNoTS (Symbol '/' '>')
          (Position (49.5,-72.75000393777269,PosENone)) Nothing "Comment")),
  ("@092345z/5L!!<*e7>{?! ",
   Right (PositionPacket PositionMsg (Symbol '/' '>')
          (Position (49.5,-72.75000393777269,PosENone)) (Just (DHMZulu (9,23,45))) " ")),
  (";LEADER   _092345z4903.50N/07201.75W>088/036",
   Right (ObjectPacket (Symbol '/' '>') -- this one eats the 088/036 course/speed
          Killed "LEADER   " (Position (49.05833333333333,-72.02916666666667,PosENone)) (DHMZulu (9,23,45)) "")),
  (";LEADER   *092345z/5L!!<*e7>7P[ ",
   Right (ObjectPacket (Symbol '/' '>')
          Live "LEADER   " (Position (49.5,-72.75000393777269,PosENone)) (DHMZulu (9,23,45)) " ")),
  (")AID #2!4903.50N/07201.75WA",
    Right (ItemPacket (Symbol '/' 'A') Live "AID #2"
           (Position (49.05833333333333,-72.02916666666667,PosENone)) "")),
  (")G/WB4APR!53  .  N\\002  .  Wd", Right (ItemPacket (Symbol '\\' 'd') Live "G/WB4APR"
                                            (Position (53.5,-2.5,PosENone)) "")),
  (")MOBIL!\\5L!!<*e79 sT", Right (ItemPacket (Symbol '\\' '9') Live
                                    "MOBIL" (Position (49.5,-72.75000393777269,PosENone)) "")),
  ("_10090556c220s004g005t077r000p000P000h50b09900wRSW",
   Right (WeatherPacket (Just (MDHM (10,9,5,56))) Nothing
          [WindDir 220, WindSpeed 4, WindGust 5, Temp 25, RainLastHour 0,
           RainLast24Hours 0, RainToday 0, Humidity 50, Baro 9900] WinAPRS WURadioShack "")),
  ("!4903.50N/07201.75W_220/004g005t077r000p000P000h50b09900wRSW",
   Right (WeatherPacket Nothing (Just (Position (49.05833333333333,-72.02916666666667,PosENone)))
          [WindDir 220, WindSpeed 4, WindGust 5, Temp 25, RainLastHour 0, RainLast24Hours 0,
           RainToday 0, Humidity 50, Baro 9900] WinAPRS WURadioShack "")),

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
          (Position (45.44433333333333,11.078,PosEPHG 4 20 3 Omni)) Nothing "6/- Lnx APRS Srv - sez. ARI VR EST")),
  ("/055816h5134.38N/00019.47W>155/023!W26!/A=000188 14.3V 27C HDOP01.0 SATS09",
   Right (PositionPacket PositionNoMsg (Symbol '/' '>')
          (Position (51.573,-0.32449999999999996,PosECourseSpeed 155 42.596000000000004)) (Just (HMS (5,58,16)))
          "!W26!/A=000188 14.3V 27C HDOP01.0 SATS09")),
  (":OH7LZB   :Testing, 1 2 3{1", Right (MessagePacket (raddr "OH7LZB")
                                         (Message "Testing, 1 2 3") "1")),
  (":OH7LZB   :Testing, 1 2 3", Right (MessagePacket (raddr "OH7LZB")
                                       (Message "Testing, 1 2 3") "")),
  (":OH7LZB   :ack1", Right (MessagePacket (raddr "OH7LZB") MessageACK "1")),
  (":OH7LZB   :rej1", Right (MessagePacket (raddr "OH7LZB") MessageNAK "1")),

  ("!I0-X;T_Wv&{-Aigate testing",
   Right (PositionPacket PositionNoTSNoMsg (Symbol 'I' '&')
          (Position (60.052010101699544,24.504507437140035,PosENone)) Nothing "igate testing")),

  ("`(_fn\"Oj/",
    Right (MicEPacket (Symbol '/' 'j') 4 (Position (33.42733333333334,-112.129,PosECourseSpeed 251 20)) "")),
  ("`(_f\DEL;\DELj/",
    Right (MicEPacket (Symbol '/' 'j') 4 (Position (33.42733333333334,-112.129,PosECourseSpeed 199 193)) "")),

  -- This came from the FAP samples, but from everything I can find,
  -- the FAP sample has interepreted the data incorrectly.
  ("!!00000066013D000028710166--------0158053201200210",
   Right (WeatherPacket Nothing Nothing [WindSpeed 0, WindDir 102, Temp (-0.16666666666666707),
                                         RainLast24Hours 0, Baro 1035, RainToday 288]
           (UnknownWeatherSW '?') WUUltimeter2000 "")),

  -- Here's some from the wild.
  ("T#7,025,023,037,008,000,00000000", Right (TelemetryPacket "7" [25,23,37,8,0] 0 "")),
  ("=/:x,r/pXZx", Right (PositionPacket PositionNoTS (Symbol '/' 'x')
                         (Position (38.64933346634255,-121.14733570299742,PosENone)) Nothing "")),
  ("@171607z3755.50N/12205.43W_000/000g000t048r000p045P001h96b10205.DsVP",
   Right (WeatherPacket (Just (DHMZulu (17,16,7))) (Just (Position (37.925,-122.0905,PosENone)))
          [WindDir 0, WindSpeed 0, WindGust 0, Temp 8.88888888888889,
           RainLastHour 0, RainLast24Hours 45, RainToday 1, Humidity 96, Baro 10205]
           (UnknownWeatherSW '.') WUDavisVantagePro "")),

  ("!3748.51N/12112.44W_270/001g001t051V136P022h88b10262OTW1",
   Right (WeatherPacket Nothing (Just (Position (37.8085,-121.20733333333334,PosENone)))
          [WindDir 270, WindSpeed 1, WindGust 1, Temp 10.555555555555555,
           Voltage 13.6, RainToday 22, Humidity 88, Baro 10262]
           OpenTracker WUOpenTrackerTW1 "")),

  ("@182018z3925.85N/11948.27W_105/003g006t045r000p000P000h48b10242L009.DsVP",
   Right (WeatherPacket (Just (DHMZulu (18,20,18)))
          (Just (Position (39.43083333333333,-119.8045,PosENone)))
          [WindDir 105,WindSpeed 3,WindGust 6,Temp 7.222222222222222,
           RainLastHour 0,RainLast24Hours 0,RainToday 0,Humidity 48,Baro 10242,
           Luminosity 9] (UnknownWeatherSW '.') WUDavisVantagePro "")),

  ("<IGATE,MSG_CNT=0,LOC_CNT=0,DIR_CNT=0,RF_CNT=0,DX=1*WR6ABD(17mi@105°)",
   Right (CapabilitiesPacket [IGATE,
                              MessageCount 0,
                              LocalCount 0,
                              Capability "DIR_CNT" "0",
                              Capability "RF_CNT" "0",
                              Capability "DX" "1*WR6ABD(17mi@105°)"])),

  -- this one is just wrong, but it's an uphill battle...
  ("<IGATE MSG_CNT=14878 LOC_CNT=57 FILL_CNT=0",
   Right (CapabilitiesPacket [IGATE,
                              MessageCount 14878,
                              LocalCount 57,
                              Capability "FILL_CNT" "0"])),

  -- 19.2 °C 68% 1013.1 mbar 0.0 m/s North
  ("$ULTW0000000002A000772788000485C80001029D013F043D00000000",
   Right (WeatherPacket Nothing Nothing [WindSpeed 0, WindDir 0,
                                         Temp 19.555555555555557, RainLast24Hours 119,
                                         Baro 1012, Humidity 66, RainToday 0]
           (UnknownWeatherSW '?') WUUltimeter2000 "")),

  ("$GPGGA,182240,3724.6788,N,12209.746,W,1,11,2.2,58.6,M,-28.4,M,,*49",
    Right (RawGPSPacket (Position (37.41131333333333,-122.16243333333334,PosENone)) (HMS (18,22,40)))),
  ("$GPGGA,182240,3724.6788,S,12209.746,E,1,11,2.2,58.6,M,-28.4,M,,*49",
    Right (RawGPSPacket (Position (-37.41131333333333,122.16243333333334,PosENone)) (HMS (18,22,40)))),
  ("$GPRMC,191608,A,3704.3616,N,12159.7271,W,000.0,000.0,151117,013.7,E*6B",
    Right (RawGPSPacket (Position (37.07269333333333,-121.99545166666667,PosENone)) (HMS (19,16,8)))),

  ("{some user defined stuff", Right (NotImplemented UserDefined "some user defined stuff"))

  ]

testFrameParser :: [TestTree]
testFrameParser =
  map (\(a, want) -> testCase (show a) $ assertEqual "" want (A.parseOnly parseFrame a)) [
  ("W6BXN-3>BEACON,qAR,AA6I-1:Turlock Amateur Radio Club APRS",
   (Right (Frame (raddr "W6BXN-3") (raddr "BEACON") ["qAR", "AA6I-1"]
           (Beacon "Turlock Amateur Radio Club APRS"))))
  ]

tests :: [TestTree]
tests = [
  testGroup "callPass"  testCallPass,
  testGroup "addrParse" testAddressParsing,
  testProperty "address round trips" (prop_roundtrips :: Address -> Bool),
  testGroup "addrSimilar" testAddrSimilar,
  testProperty "addrSimilar" propAddrSimilar,
  testCase "frame parsing" testChristmasMsg,
  localOption (QC.QuickCheckTests 1000) $ testProperty "address validation" propValidAddress,
  testGroup "base91" testBase91,
  testCase "no dup packet types" $ testNoDupMapping (identifyPacket.chr.fromIntegral :: Word8 -> PacketType),

  testGroup "timestamp parsing" testTimestampParser,
  testGroup "weather parsing" testWeatherParser,
  testGroup "mega parser" testMegaParser,
  testGroup "frame parser" testFrameParser
  ]
