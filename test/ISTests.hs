{-# LANGUAGE OverloadedStrings #-}

module ISTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Attoparsec.Text as A

import APRS.IS
import APRS.Types (Address)

raddr :: String -> Address
raddr = read

testIDParser :: [TestTree]
testIDParser =
  map (\(a, want) -> testCase (show a) $ assertEqual "" want (A.parseOnly parseIdentification a)) [
  ("user KG6HWF-9 pass 11223 vers haprs 0.1 filter r/1", (Left "Failed reading: invalid callpass")),
  ("user KG6HWF-9", (Left "call pass: not enough input")),

  ("user KG6HWF-9 pass 22955 vers haprs 0.1 filter r/1",
   (Right (ID (raddr "KG6HWF-9") (Just ("haprs", "0.1")) Nothing "filter r/1"))),
  ("user KG6HWF-9 pass 22955 filter r/1",
    (Right (ID (raddr "KG6HWF-9") Nothing Nothing "filter r/1"))),
  ("user KG6HWF-9 pass 22955",
    (Right (ID (raddr "KG6HWF-9") Nothing Nothing ""))),
  ("user KG6HWF-9 pass 22955 UDP 8425 filter r/1",
   (Right (ID (raddr "KG6HWF-9") Nothing (Just 8425) "filter r/1"))),
  ("user KG6HWF-9 pass 22955 vers haprs 0.1 UDP 1384 filter r/1",
    (Right (ID (raddr "KG6HWF-9") (Just ("haprs", "0.1")) (Just 1384) "filter r/1")))
  ]

tests :: [TestTree]
tests = [
  testGroup "id parser" testIDParser
  ]
