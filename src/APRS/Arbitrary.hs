{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module APRS.Arbitrary (
  ArbitraryCall(..)
  , ArbitrarySSID(..)
  , addrChars
  ) where

import Data.Semigroup ((<>))
import Data.String (fromString)
import Control.Applicative (liftA3)
import Test.QuickCheck
import qualified Data.Text as T

import APRS.IS
import APRS.Types

addrChars :: [Char]
addrChars = ['A'..'Z'] ++ ['0'..'9']

arbitraryText :: String -> (Int, Int) -> Gen T.Text
arbitraryText chars range = do
    l <- choose range
    a <- vectorOf l $ elements chars
    pure (fromString $ take l a)

arbitraryTextList :: String -> (Int, Int) -> Gen [T.Text]
arbitraryTextList chars range = do
  l <- choose (1, 7)
  a <- vectorOf l $ arbitraryText chars range
  pure $ take l a

instance Arbitrary Position where
  arbitrary = do
    lat <- choose (-90, 90)
    lon <- choose (-180, 180)
    alt <- arbitrary
    ext <- arbitrary
    pure $ Position (lon, lat, alt, ext)

instance Arbitrary Directivity where arbitrary = arbitraryBoundedEnum

instance Arbitrary PosExtension where
  arbitrary = oneof [
    PosECourseSpeed <$> arbitrary <*> arbitrary,
    PosEPHG <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
    pure PosERNG,
    pure PosEDFS,
    pure PosETypeDesc,
    pure PosENone
    ]

instance Arbitrary WeatherParam where
  arbitrary = oneof [
    WindDir <$> choose (1, 360),
    WindSpeed <$> choose (0, 500),
    WindGust <$> choose (0, 500),
    Temp <$> choose (-50, 50),
    RainLastHour <$> choose (0, 100),
    RainLast24Hours <$> choose (0, 100),
    RainToday <$> choose (0, 100),
    Humidity <$> choose (0, 100),
    Baro <$> choose (0, 1000),
    Voltage <$> choose (0, 250),
    WaterLevel <$> choose (0, 1000),
    Luminosity <$> choose (0, 100000),
    Snowfall <$> choose (0, 100),
    RawRain <$> choose (0, 1000000),
    NoData <$> arbitrary
    ]

instance Arbitrary FilterItem where
  arbitrary = frequency [
    (1, NotFilter <$> arbitrary `suchThat` (not.isNegative)),
    (10, RangeFilter <$> (truncPos <$> arbitrary) <*> arbitrary),
    (10, PrefixFilter <$> ws),
    (10, BudlistFilter <$> listOf1 arbitrary),
    (10, ObjectFilter <$> ws),
    (10, StrictObjectFilter <$> ws),
    (10, TypeFilter . T.unpack <$> (arbitraryText "poimqstunw" (1,9)) <*> arbitrary),
    (10, SymbolFilter <$> w <*> w <*> w),
    (10, DigiFilter <$> listOf1 arbitrary),
    (10, (AreaFilter <$> choose (-90, 90)
           <*> choose (-90, 90)
           <*> choose (-180, 180)
           <*> choose (-180, 180)) `suchThat` isValidAF),
    (10, EntryStationFilter <$> listOf1 arbitrary),
    (10, GroupMessageFilter <$> listOf1 arbitrary),
    (10, UnprotoFilter <$> listOf1 arbitrary),
    (10, QConsFilter <$> listOf (elements "CXUoOsRrZI") <*> arbitrary),
    (10, MyRangeFilter <$> arbitrary),
    (10, FriendRangeFilter <$> arbitrary <*> arbitrary)
    ]

    where ws = arbitraryTextList addrChars (1, 7)
          w = listOf (elements addrChars)
          isNegative (NotFilter _) = True
          isNegative _ = False

          isValidAF (AreaFilter latN lonW latS lonE) = latN >= latS && lonW <= lonE
          isValidAF _ = True

instance Arbitrary Filter where
  arbitrary = Filter <$> listOf1 arbitrary
  shrink (Filter a) = Filter <$> ss a
    where ss [_] = [] -- never shrink to zero filter items
          ss l = map (flip dropn l) [0..length l - 1]
            where dropn n = take n <> drop (succ n)

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
    pure $ either undefined id $ address c s

instance Arbitrary Frame where
  arbitrary = Frame <$> arbitrary <*> arbitrary <*> arbitraryTextList addrChars (1,7) <*> arbitrary

instance Arbitrary MessageInfo where
          truncPos (Position (a, b, _, _)) = Position (a,b,0,PosENone)
  arbitrary = frequency [
    (60, Message <$> arbitraryText addrChars (1,7)),
    (1, pure MessageACK),
    (1, pure MessageNAK)
    ]

instance Arbitrary Capability where
  arbitrary = frequency [
    (5, pure IGATE),
    (5, MessageCount <$> arbitrary),
    (5, LocalCount <$> arbitrary),
    (5, Capability <$> arbitraryText addrChars (1,7) <*> arbitraryText addrChars (1,7))
    ]

instance Arbitrary APRSPacket where
  arbitrary = frequency [
    (20, MessagePacket <$> arbitrary <*> arbitrary <*> (T.pack <$> arbitrary)),
    (5, CapabilitiesPacket <$> arbitrary)
    ]

instance Arbitrary Timestamp where
  arbitrary = oneof [
    liftA3 (\a b c -> DHMLocal (a,b,c)) d h m,
    liftA3 (\a b c -> DHMZulu (a,b,c)) d h m,
    liftA3 (\a b c -> HMS (a,b,c)) h m s,
    (\a b c d' -> MDHM (a,b,c,d')) <$> choose (1, 12) <*> d <*> h <*> m
    ]

    where d = choose (0, 31)
          h = choose (0, 23)
          m = choose (0, 59)
          s = choose (0, 59)
