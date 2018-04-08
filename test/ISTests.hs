{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module ISTests where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T

import APRS.IS
import APRS.Types (Address, Frame, Position(..), PosExtension(..))
import APRS.Arbitrary ()

raddr :: String -> Address
raddr = read

testIDParser :: [TestTree]
testIDParser =
  map (\(a, want) -> testCase (show a) $ assertEqual "" want (A.parseOnly parseIdentification a)) [
  ("user KG6HWF-9 pass 11223 vers haprs 0.1 filter r/1", (Left "Failed reading: invalid callpass")),
  ("user KG6HWF-9", (Left "call pass: not enough input")),

  ("user KG6HWF-9 pass -1 vers haprs 0.1 filter r/1",
   (Right (id' (raddr "KG6HWF-9") (Just ("haprs", "0.1")) Nothing "filter r/1"))),
  ("user KG6HWF-9 pass 22955 vers haprs 0.1 filter r/1",
   (Right (id' (raddr "KG6HWF-9") (Just ("haprs", "0.1")) Nothing "filter r/1"))),
  ("user KG6HWF-9 pass 22955 filter r/1",
    (Right (id' (raddr "KG6HWF-9") Nothing Nothing "filter r/1"))),
  ("user KG6HWF-9 pass 22955",
    (Right (id' (raddr "KG6HWF-9") Nothing Nothing ""))),
  ("user KG6HWF-9 pass 22955 UDP 8425 filter r/1",
   (Right (id' (raddr "KG6HWF-9") Nothing (Just 8425) "filter r/1"))),
  ("user KG6HWF-9 pass 22955 vers haprs 0.1 UDP 1384 filter r/1",
    (Right (id' (raddr "KG6HWF-9") (Just ("haprs", "0.1")) (Just 1384) "filter r/1")))
  ]

  where id' a b c d = ID a b c d undefined

propValidIDRW :: Frame -> Bool
propValidIDRW = let (Right (ID _ _ _ _ v)) = A.parseOnly parseIdentification "user KG6HWF-9 pass 22955" in
                  v

propValidIDRO :: Frame -> Bool
propValidIDRO = let (Right (ID _ _ _ _ v)) = A.parseOnly parseIdentification "user KG6HWF-9 pass -1" in
                  not.v

testFilterParser :: [TestTree]
testFilterParser =
  map (\(a, want) -> testCase (show a) $ assertEqual "" want (A.parseOnly parseFilter a)) [
  ("filter r/37.335278/-121.891944/50",
   Right (Filter [RangeFilter (Position (-121.891944,37.335278,0.0,PosENone)) 50.0])),
  ("filter p/KG/K6",
   Right (Filter [PrefixFilter ["KG", "K6"]])),
  ("filter b/KG6HWF/K6MGD",
   Right (Filter [BudlistFilter $ map raddr ["KG6HWF", "K6MGD"]])),
  ("filter o/BAYLANDS/SOMETHING",
   Right (Filter [ObjectFilter ["BAYLANDS", "SOMETHING"]])),
  ("filter os/BAYLANDS/SOMETHING",
   Right (Filter [StrictObjectFilter ["BAYLANDS", "SOMETHING"]])),
  ("filter t/pow",
   Right (Filter [TypeFilter "pow" Nothing])),
  ("filter t/pow/KG6HWF/13",
   Right (Filter [TypeFilter "pow" (Just (raddr "KG6HWF", 13))])),
  ("filter s/->",
   Right (Filter [SymbolFilter "->" "" ""])),
  ("filter s//#",
   Right (Filter [SymbolFilter "" "#" ""])),
  ("filter s//#/T",
   Right (Filter [SymbolFilter "" "#" "T"])),
  ("filter d/KG6HWF-1/KG6HWF-3",
   Right (Filter [DigiFilter $ map raddr ["KG6HWF-1", "KG6HWF-3"]])),
  ("filter a/38/37/-122/-121",
   Right (Filter [AreaFilter 38 37 (-122) (-121)])),
  ("filter e/KG6HWF-3/K6MGD-5",
   Right (Filter [EntryStationFilter $ map raddr ["KG6HWF-3", "K6MGD-5"]])),
  ("filter g/KG6HWF/K6MGD/KI6IQI",
   Right (Filter [GroupMessageFilter $ map raddr ["KG6HWF", "K6MGD", "KI6IQI"]])),
  ("filter u/KG6HWF-3",
   Right (Filter [UnprotoFilter $ map raddr ["KG6HWF-3"]])),
  ("filter q/C", Right (Filter [QConsFilter "C" False])),
  ("filter q/rR", Right (Filter [QConsFilter "rR" False])),
  ("filter q//I", Right (Filter [QConsFilter "" True])),
  ("filter m/13", Right (Filter [MyRangeFilter 13])),
  ("filter f/K6MGD/19.3", Right (Filter [FriendRangeFilter (raddr "K6MGD") 19.3])),
  ("filter m/200 -p/CW",
   Right (Filter [MyRangeFilter 200.0,NotFilter (PrefixFilter ["CW"])])),
  ("filter r/33/-97/200 t/n",
   Right (Filter [RangeFilter (Position (-97.0,33.0,0.0,PosENone)) 200.0,TypeFilter "n" Nothing]))
  ]

propParseRoundTrip :: (Eq a, Show a) => A.Parser a -> a -> Bool
propParseRoundTrip f i = A.parseOnly f ((T.pack.show) i) == Right i

tests :: [TestTree]
tests = [
  testGroup "id parser" testIDParser,
  testProperty "callsign validation (read-write)" propValidIDRW,
  testProperty "callsign validation (read-only)" propValidIDRO,
  testGroup "filter parser" testFilterParser,

  testProperty "filter round tripping" (propParseRoundTrip parseFilter),
  testProperty "filter item round tripping" (withMaxSuccess 1000 $ propParseRoundTrip parseFilterItem)
  ]
