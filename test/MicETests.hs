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

tests :: [TestTree]
tests = [
  testGroup "micELonD" testMicELonD,
  testGroup "micELonM" testMicELonM
  ]
