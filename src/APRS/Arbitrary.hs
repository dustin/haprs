{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module APRS.Arbitrary (
  ArbitraryCall(..)
  , ArbitrarySSID(..)
  , addrChars
  ) where

import Data.String (fromString)
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
    pure $ Position (lon, lat, 0, PosENone)

instance Arbitrary FilterItem where
  arbitrary = frequency [
    (1, NotFilter <$> arbitrary),
    (10, RangeFilter <$> arbitrary <*> arbitrary),
    (10, PrefixFilter <$> ws),
    (10, BudlistFilter <$> listOf1 arbitrary),
    (10, ObjectFilter <$> ws),
    (10, StrictObjectFilter <$> ws),
    (10, TypeFilter . T.unpack <$> (arbitraryText "poimqstunw" (1,9)) <*> arbitrary),
    (10, SymbolFilter <$> w <*> w <*> w),
    (10, DigiFilter <$> listOf1 arbitrary),
    (10, AreaFilter <$> choose (-90, 90)
         <*> choose (-90, 90)
         <*> choose (-180, 180)
         <*> choose (-180, 180)),
    (10, EntryStationFilter <$> listOf1 arbitrary),
    (10, GroupMessageFilter <$> listOf1 arbitrary),
    (10, UnprotoFilter <$> listOf1 arbitrary),
    (10, QConsFilter <$> listOf (elements "CXUoOsRrZI") <*> arbitrary),
    (10, MyRangeFilter <$> arbitrary),
    (10, FriendRangeFilter <$> arbitrary <*> arbitrary)
    ]

    where ws = arbitraryTextList addrChars (1, 7)
          w = listOf1 (elements addrChars)

instance Arbitrary Filter where
  arbitrary = Filter <$> listOf1 arbitrary

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
  arbitrary = Message <$> arbitraryText addrChars (1,7)

instance Arbitrary APRSPacket where
  arbitrary = frequency [
    (1, MessagePacket <$> arbitrary <*> arbitrary <*> (T.pack <$> arbitrary))
    ]
