{-# LANGUAGE OverloadedStrings #-}

module MicETests (tests) where

import APRS.MicE
import Test.Tasty
import Test.Tasty.HUnit

testMicELonD :: [TestTree]
testMicELonD =
  map (\(a@(c,off), want) -> testCase (show a ++ " -> " ++ show want) $ assertEqual "" want (micELonD c off)) [
  (('v', 100), 0),
  (('\DEL', 100), 9),
  (('&', 0), 10),
  (('~', 0), 98),
  (('\DEL', 0), 99),
  (('m', 100), 101),
  (('j', 100), 178)
  ]

testMicELonM :: [TestTree]
testMicELonM =
  map (\(a, want) -> testCase (show a ++ " -> " ++ show want) $ assertEqual "" want (micELonM a)) [
  ('X', 0),
  (']', 5),
  ('*', 14),
  ('T', 56),
  ('W', 59)
  ]

testMicEDest :: [TestTree]
testMicEDest =
  map (\(a@(c,s), want) -> testCase (show a ++ " -> " ++ show want) $ assertEqual "" want (micEDest c s)) [
  (("S32U6T",""), (33.42733333333334, 4, 0, (-1), 0)),
  (("S32UVT",""), (33.42733333333334, 4, 100, (-1), 0)),
  (("S325V4",""), (-33.42733333333334, 4, 100, 1, 0))
  ]

testDigits :: Assertion
testDigits =
  mapM_ (\(a, want) -> assertEqual (show a ++ " -> " ++ show want) want (digit a)) [
  ('0', 0),
  ('9', 9),
  ('A', 0),
  ('J', 9),
  ('K', 0),
  ('L', 0),
  ('P', 0),
  ('Q', 1),
  ('Y', 9)
  ]

tests :: [TestTree]
tests = [
  testGroup "micELonD" testMicELonD,
  testGroup "micELonM" testMicELonM,
  testGroup "micEDest" testMicEDest,
  testCase "digits" testDigits
  ]
