{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-type-defaults #-}

module APRSTests (tests) where

import APRS.Types
import APRS.Arbitrary

import Data.Char (chr)
import Data.Either (isRight, either)
import Data.String (IsString, fromString)
import Data.Word (Word8)
import Data.Semigroup (mconcat)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import qualified Data.Attoparsec.Text as A
import qualified Data.Set as Set

must :: Either String a -> a
must = either undefined id

instance IsString Address where fromString = read

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

propUnAddress :: Address -> Bool
propUnAddress a = Right a == (uncurry address . unAddress) a

testAddrSimilar :: [TestTree]
testAddrSimilar =
  map (\(a, b) -> testCase (a ++ " ≈ " ++ b) $ assertBool "" (raddr a ≈ raddr b)) [
  ("KG6HWF", "KG6HWF"),
  ("KG6HWF-9", "KG6HWF"),
  ("KG6HWF", "KG6HWF-11"),
  ("KG6HWF-9", "KG6HWF-11")]

  where raddr :: String -> Address
        raddr = read

-- Same as the above, but as a property test.
propAddrSimilar :: ArbitraryCall -> ArbitrarySSID -> ArbitrarySSID -> Bool
propAddrSimilar (ArbitraryCall c) (ArbitrarySSID s1) (ArbitrarySSID s2) =
  ma c s1 ≈ ma c s2
  where ma c' s' = must (address c' s')

propElemish :: [Address] -> [Address] -> ArbitraryCall -> ArbitrarySSID -> ArbitrarySSID -> Bool
propElemish l1 l2 (ArbitraryCall c) (ArbitrarySSID s1) (ArbitrarySSID s2) =
  must (address c s1) `elemish` mconcat [l1, [must (address c s2)], l2]

testBase91 :: [TestTree]
testBase91 =
  map (\(a, want) -> testCase (show a ++ " -> " ++ show want) $ assertEqual "" want (decodeBase91 a)) [
  (['\0', '\0', '\0', '\0'], -25144152),
  (['\1', '\0', '\0', '\0'], -24390581),
  (['\1', '\0', '\0', '\1'], -24390580),
  (['\1', '\0', '\xff', '\1'], -24367375),
  ("\"4T", 10061),
  ("<*e7", 20346417 + 74529 + 6188 + 22)]

prop_roundtrips :: (Show a, Read a, Eq a) => a -> Bool
prop_roundtrips x = read (show x) == x

christmasMsg :: String
christmasMsg = "KG6HWF>APX200,WIDE1-1,WIDE2-1:=3722.1 N/12159.1 W-Merry Christmas!"

rframe :: String -> Frame
rframe = must . A.parseOnly parseFrame . fromString

testChristmasMsg :: Assertion
testChristmasMsg =
  assertEqual "christmas parsing" "KG6HWF" $ source $ rframe christmasMsg
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

-- Convenience for testing things that map from lowercase chars
newtype LowChar = LowChar Char deriving (Ord, Show, Eq)

instance Bounded LowChar where
  minBound = LowChar 'a'
  maxBound = LowChar 'z'

fromLowChar :: LowChar -> Char
fromLowChar (LowChar c) = c

instance Enum LowChar where
  fromEnum = fromEnum . fromLowChar
  toEnum = LowChar . toEnum
  enumFrom     x   = enumFromTo     x maxBound
  enumFromThen x y = enumFromThenTo x y bound
    where
      bound | fromEnum y >= fromEnum x = maxBound
            | otherwise                = minBound


testNoDupMapping :: (Bounded a, Enum a, Show b, Ord b) => (a -> b) -> Assertion
testNoDupMapping f = case foldr findDup (Right Set.empty) [minBound..] of
                       Left x -> assertBool ("Duplicate value found: " ++ show x) True
                       Right _ -> pure ()
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
  map (\(a, want) -> testCase (show a) $ assertEqual "" want (A.parseOnly (bodyParser "S32UVT") a)) [
  ("!4903.50N/07201.75W-Test 001234",
   Right (PositionPacket PositionNoTSNoMsg (Symbol '/' '-')
          (Position (49.05833333333333,-72.02916666666667,0,PosENone)) Nothing "Test 001234")),
  ("!0000.00N\\00000.00W-Unknown pos",
   Right (PositionPacket PositionNoTSNoMsg (Symbol '\\' '-')
          (Position (0,0,0,PosENone)) Nothing "Unknown pos")),
  ("!4903.50N/07201.75W-Test /A=001234", -- TODO:  Parse out the altitude (1234 feet, anywhere in comment)
   Right (PositionPacket PositionNoTSNoMsg (Symbol '/' '-')
          (Position (49.05833333333333,-72.02916666666667,0,PosENone)) Nothing "Test /A=001234")),
  ("!49  .  N/072  .  W-",
   Right (PositionPacket PositionNoTSNoMsg (Symbol '/' '-')
          (Position (49.5,-72.5,0,PosENone)) Nothing "")),
  ("/092345z4903.50N/07201.75W>Test1234",
   Right (PositionPacket PositionNoMsg (Symbol '/' '>')
          (Position (49.05833333333333,-72.02916666666667,0,PosENone)) (Just (DHMZulu (9,23,45))) "Test1234")),
  ("@092345/4903.50N/07201.75W>Test1234",
   Right (PositionPacket PositionMsg (Symbol '/' '>')
          (Position (49.05833333333333,-72.02916666666667,0,PosENone)) (Just (DHMLocal (9,23,45))) "Test1234")),
  ("=/5L!!<*e7> sTComment",
   Right (PositionPacket PositionNoTS (Symbol '/' '>')
          (Position (49.5,-72.75000393777269,0,PosENone)) Nothing "Comment")),
  ("@092345z/5L!!<*e7>{?! ",
   Right (PositionPacket PositionMsg (Symbol '/' '>')
          (Position (49.5,-72.75000393777269,0,PosENone)) (Just (DHMZulu (9,23,45))) " ")),
  (";LEADER   _092345z4903.50N/07201.75W>088/036",
   Right (ObjectPacket (Symbol '/' '>') -- this one eats the 088/036 course/speed
          Killed "LEADER   " (Position (49.05833333333333,-72.02916666666667,0,PosENone)) (DHMZulu (9,23,45))
         (ObjText ""))),
  (";LEADER   *092345z/5L!!<*e7>7P[ ",
   Right (ObjectPacket (Symbol '/' '>')
          Live "LEADER   " (Position (49.5,-72.75000393777269,0,PosENone)) (DHMZulu (9,23,45))
          (ObjText " "))),
  (")AID #2!4903.50N/07201.75WA",
    Right (ItemPacket (Symbol '/' 'A') Live "AID #2"
           (Position (49.05833333333333,-72.02916666666667,0,PosENone)) "")),
  (")G/WB4APR!53  .  N\\002  .  Wd", Right (ItemPacket (Symbol '\\' 'd') Live "G/WB4APR"
                                            (Position (53.5,-2.5,0,PosENone)) "")),
  (")MOBIL!\\5L!!<*e79 sT", Right (ItemPacket (Symbol '\\' '9') Live
                                    "MOBIL" (Position (49.5,-72.75000393777269,0,PosENone)) "")),
  ("_10090556c220s004g005t077r000p000P000h50b09900wRSW",
   Right (WeatherPacket (Just (MDHM (10,9,5,56))) Nothing
          [WindDir 220, WindSpeed 4, WindGust 5, Temp 25, RainLastHour 0,
           RainLast24Hours 0, RainToday 0, Humidity 50, Baro 9900] WinAPRS WURadioShack "")),
  ("!4903.50N/07201.75W_220/004g005t077r000p000P000h50b09900wRSW",
   Right (WeatherPacket Nothing (Just (Position (49.05833333333333,-72.02916666666667,0,PosENone)))
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
          (Position (45.44433333333333,11.078,0,PosEPHG 4 20 3 Omni)) Nothing "6/- Lnx APRS Srv - sez. ARI VR EST")),
  ("/055816h5134.38N/00019.47W>155/023!W26!/A=000188 14.3V 27C HDOP01.0 SATS09",
   Right (PositionPacket PositionNoMsg (Symbol '/' '>')
          (Position (51.573,-0.32449999999999996,0,PosECourseSpeed 155 42.596000000000004)) (Just (HMS (5,58,16)))
          "!W26!/A=000188 14.3V 27C HDOP01.0 SATS09")),
  (":OH7LZB   :Testing, 1 2 3{1", Right (MessagePacket "OH7LZB"
                                         (Message "Testing, 1 2 3") "1")),
  (":OH7LZB   :Testing, 1 2 3", Right (MessagePacket "OH7LZB"
                                       (Message "Testing, 1 2 3") "")),
  (":OH7LZB   :ack1", Right (MessagePacket "OH7LZB" MessageACK "1")),
  (":OH7LZB   :rej1", Right (MessagePacket "OH7LZB" MessageNAK "1")),

  ("!I0-X;T_Wv&{-Aigate testing",
   Right (PositionPacket PositionNoTSNoMsg (Symbol 'I' '&')
          (Position (60.052010101699544,24.504507437140035,0,PosENone)) Nothing "igate testing")),

  ("`(_fn\"Oj/",
    Right (MicEPacket (Symbol '/' 'j') 4 (Position (33.42733333333334,-112.129,0,PosECourseSpeed 251 20)) "")),
  ("`(_f\DEL;\DELj/",
    Right (MicEPacket (Symbol '/' 'j') 4 (Position (33.42733333333334,-112.129,0,PosECourseSpeed 199 193)) "")),

  -- This came from the FAP samples, but from everything I can find,
  -- the FAP sample has interepreted the data incorrectly.
  ("!!00000066013D000028710166--------0158053201200210",
   Right (WeatherPacket Nothing Nothing [WindSpeed 0, WindDir 102, Temp (-0.16666666666666707),
                                         RainLast24Hours 0, Baro 1035, RainToday 288]
           (UnknownWeatherSW '?') WUUltimeter2000 "")),

  -- Here's some from the wild.
  ("T#7,025,023,037,008,000,00000000", Right (TelemetryPacket "7" [25,23,37,8,0] 0 "")),
  ("=/:x,r/pXZx", Right (PositionPacket PositionNoTS (Symbol '/' 'x')
                         (Position (38.64933346634255,-121.14733570299742,0,PosENone)) Nothing "")),
  ("@171607z3755.50N/12205.43W_000/000g000t048r000p045P001h96b10205.DsVP",
   Right (WeatherPacket (Just (DHMZulu (17,16,7))) (Just (Position (37.925,-122.0905,0,PosENone)))
          [WindDir 0, WindSpeed 0, WindGust 0, Temp 8.88888888888889,
           RainLastHour 0, RainLast24Hours 45, RainToday 1, Humidity 96, Baro 10205]
           (UnknownWeatherSW '.') WUDavisVantagePro "")),

  ("!3748.51N/12112.44W_270/001g001t051V136P022h88b10262OTW1",
   Right (WeatherPacket Nothing (Just (Position (37.8085,-121.20733333333334,0,PosENone)))
          [WindDir 270, WindSpeed 1, WindGust 1, Temp 10.555555555555555,
           Voltage 13.6, RainToday 22, Humidity 88, Baro 10262]
           OpenTracker WUOpenTrackerTW1 "")),

  ("@182018z3925.85N/11948.27W_105/003g006t045r000p000P000h48b10242L009.DsVP",
   Right (WeatherPacket (Just (DHMZulu (18,20,18)))
          (Just (Position (39.43083333333333,-119.8045,0,PosENone)))
          [WindDir 105,WindSpeed 3,WindGust 6,Temp 7.222222222222222,
           RainLastHour 0,RainLast24Hours 0,RainToday 0,Humidity 48,Baro 10242,
           Luminosity 9] (UnknownWeatherSW '.') WUDavisVantagePro "")),
  ("@182018z3925.85N/11948.27W_105/003g006t045r000F013P000h48b10242l009.DsVP",
   Right (WeatherPacket (Just (DHMZulu (18,20,18)))
          (Just (Position (39.43083333333333,-119.8045,0,PosENone)))
          [WindDir 105,WindSpeed 3,WindGust 6,Temp 7.222222222222222,
           RainLastHour 0,WaterLevel 1.3,RainToday 0,Humidity 48,Baro 10242,
           Luminosity 1009] (UnknownWeatherSW '.') WUDavisVantagePro "")),

  ("!3748.51N/12112.44W_270/001g001t051V136P022h88b10262s111OTW1",
   Right (WeatherPacket Nothing (Just (Position (37.8085,-121.20733333333334,0,PosENone)))
          [WindDir 270, WindSpeed 1, WindGust 1, Temp 10.555555555555555,
           Voltage 13.6, RainToday 22, Humidity 88, Baro 10262, Snowfall 111]
           OpenTracker WUOpenTrackerTW1 "")),

  ("!3748.51N/12112.44W_270/001g001t051V136P022h88b10262s111",
   Right (WeatherPacket Nothing (Just (Position (37.8085,-121.20733333333334,0,PosENone)))
          [WindDir 270, WindSpeed 1, WindGust 1, Temp 10.555555555555555,
           Voltage 13.6, RainToday 22, Humidity 88, Baro 10262, Snowfall 111]
           (UnknownWeatherSW '?') (UnknownWeatherUnit "??") "")),

  ("!3748.51N/12112.44W_270/001g001t051V136P022h88b10262#111OTW1",
   Right (WeatherPacket Nothing (Just (Position (37.8085,-121.20733333333334,0,PosENone)))
          [WindDir 270, WindSpeed 1, WindGust 1, Temp 10.555555555555555,
           Voltage 13.6, RainToday 22, Humidity 88, Baro 10262, RawRain 111]
           OpenTracker WUOpenTrackerTW1 "")),

  (";KO6TH-WX *170809z3857.54N/12106.64W_000/001g000t044r000p000P000h099b00000",
   Right (ObjectPacket (Symbol '/' '_') Live "KO6TH-WX "
          (Position (38.959,-121.11066666666666,0,PosENone)) (DHMZulu (17,8,9))
          (ObjWeather [WindGust 0, Temp 6.666666666666667, RainLastHour 0, RainLast24Hours 0,
                       RainToday 0, Humidity 9]))),

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
    Right (RawGPSPacket (Position (37.41131333333333,-122.16243333333334,0,PosENone)) (HMS (18,22,40)))),
  ("$GPGGA,182240,3724.6788,S,12209.746,E,1,11,2.2,58.6,M,-28.4,M,,*49",
    Right (RawGPSPacket (Position (-37.41131333333333,122.16243333333334,0,PosENone)) (HMS (18,22,40)))),
  ("$GPRMC,191608,A,3704.3616,N,12159.7271,W,000.0,000.0,151117,013.7,E*6B",
    Right (RawGPSPacket (Position (37.07269333333333,-121.99545166666667,0,PosENone)) (HMS (19,16,8)))),

  ("{some user defined stuff", Right (NotUnderstoodPacket "{some user defined stuff"))

  ]

testFrameParser :: [TestTree]
testFrameParser =
  map (\(a, want) -> testCase (show a) $ assertEqual "" want (A.parseOnly parseFrame a)) [
  ("W6BXN-3>BEACON,qAR,AA6I-1:Turlock Amateur Radio Club APRS",
   (Right (Frame "W6BXN-3" "BEACON" ["qAR", "AA6I-1"]
           (NotUnderstoodPacket "Turlock Amateur Radio Club APRS")))),

  ("KE6BEA>SXQTXV,W6CX-3*,WIDE2-1,qAR,K6RPT:'2Z4l k/]\"3s}Sean's Truck Fairfield CA",
   Right (Frame "KE6BEA" "SXQTXV" ["W6CX-3*","WIDE2-1","qAR","K6RPT"]
          (MicEPacket (Symbol ']' '/') 7
           (Position (38.24766666666667,-122.03733333333334,1,PosECourseSpeed 79 0.0))
           "Sean's Truck Fairfield CA"))),

  ("KD0YBR>SW4PXP,WB6TMS-6*,WIDE1*,WIDE2-1,qAR,W6PKT-5:`2_A!I:>/]\"3x}=",
    Right (Frame "KD0YBR" "SW4PXP" ["WB6TMS-6*","WIDE1*","WIDE2-1","qAR","W6PKT-5"]
           (MicEPacket (Symbol '/' '>') 6
            (Position (37.68,-122.12283333333333,6.0,PosECourseSpeed 130 54.0)) "="))),

  ("WA6TA-7>S6TPTV,W6TUW-3*,WIDE2-1,qAR,AC6SL-4:`1K\\l +K\\>\"45}^",
   Right (Frame "WA6TA-7" "S6TPTV" ["W6TUW-3*","WIDE2-1","qAR","AC6SL-4"]
          (MicEPacket (Symbol '\\' 'K') 5
           (Position (36.67433333333334,-121.794,30.0,PosECourseSpeed 15 0.0)) "^"))),
  ("W6TDR>3W0PVQ,W6BXN-3*,qAR,W6SRR-3:`/O7ppuj/`\"55}146.820MHz T141 -060_%",
    Right (Frame "W6TDR" "3W0PVQ" ["W6BXN-3*","qAR","W6SRR-3"]
           (MicEPacket (Symbol '/' 'j') 2
            (Position (37.01016666666667,-119.8545,121.0,PosECourseSpeed 89 48.0))
            "146.820MHz T141 -060_%"))),

  -- A garbage packet
  ("WA6EWV-3>ID,SNOW,qAR,KJ6NKR-2:WA6EWV-3/R NONE/D WA6EWV-5/N",
   (Right (Frame "WA6EWV-3" "ID" ["SNOW","qAR","KJ6NKR-2"]
            (NotUnderstoodPacket "WA6EWV-3/R NONE/D WA6EWV-5/N"))))
  ]

tests :: [TestTree]
tests = [
  testGroup "callPass"  testCallPass,
  testGroup "addrParse" testAddressParsing,
  testProperty "address round trips" (prop_roundtrips :: Address -> Bool),
  testProperty "address/unAddress" propUnAddress,
  testGroup "addrSimilar" testAddrSimilar,
  testProperty "addrSimilar" propAddrSimilar,
  testProperty "addr elemish" propElemish,
  testProperty "addr not elemish" propNotElemish,
  testCase "frame parsing" testChristmasMsg,
  localOption (QC.QuickCheckTests 1000) $ testProperty "address validation" propValidAddress,
  testGroup "base91" testBase91,
  testCase "no dup packet types" $ testNoDupMapping (identifyPacket.chr.fromIntegral :: Word8 -> PacketType),
  testCase "no dup weather sw" $ testNoDupMapping (lookupWeatherSW.fromLowChar :: LowChar -> WeatherSW),

  testGroup "timestamp parsing" testTimestampParser,
  testGroup "weather parsing" testWeatherParser,
  testGroup "mega parser" testMegaParser,
  testGroup "frame parser" testFrameParser
  ]
