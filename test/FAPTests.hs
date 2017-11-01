{-# LANGUAGE OverloadedStrings #-}

module FAPTests (tests) where

import APRS.Types

import Data.Aeson
import Data.Maybe
import Text.Read (readEither)
import qualified Data.ByteString.Lazy as B

import Test.HUnit (assertEqual, assertFailure)
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit


data Digipeater = Digipeater {
  _wasdigied :: Int
  , _call :: !String
  } deriving (Show)

instance FromJSON Digipeater where
  parseJSON = withObject "Digipeater" $ \v -> Digipeater
    <$> v .: "wasdigied"
    <*> v .: "call"

data FAPResult = FAPResult {
  _symbolCode :: Maybe String
  , _symbolTable :: Maybe String
  , srcCallsign :: Maybe String
  , dstCallsign :: Maybe String
  , _latitude :: Maybe Double
  , _longitude :: Maybe Double
  , _posResolution :: Maybe Double
  , _posAmbiguity :: Maybe Double
  , body :: Maybe String
  , _format :: Maybe String
  , _messaging :: Maybe Int
  , _origPacket :: Maybe String
  , _typ :: Maybe String
  , _header :: Maybe String
  , _digipeaters :: Maybe [Digipeater]
  } deriving (Show)

instance FromJSON FAPResult where
  parseJSON = withObject "FAPResult" $ \v -> FAPResult
    <$> v .:? "symbolcode"
    <*> v .:? "symboltable"
    <*> v .:? "srccallsign"
    <*> v .:? "dstcallsign"
    <*> v .:? "latitude"
    <*> v .:? "longitude"
    <*> v .:? "posresolution"
    <*> v .:? "posambiguity"
    <*> v .:? "body"
    <*> v .:? "format"
    <*> v .:? "messaging"
    <*> v .:? "origPacket"
    <*> v .:? "type"
    <*> v .:? "header"
    <*> v .:? "digipeaters"

data FAPTest = FAPTest {
  src :: !String
  , result :: Maybe FAPResult
  , _failed :: Int
  } deriving (Show)

instance FromJSON FAPTest where
    parseJSON = withObject "FAPTest" $ \v -> FAPTest
        <$> v .: "src"
        <*> v .:? "result"
        <*> v .:? "failed" .!= 0

fapTest :: FAPTest -> Test
fapTest (FAPTest _ _ 1) = testGroup "" [ ] -- ignore failed ones
fapTest f = case readEither (src f) :: Either String Frame of
              Left e -> testCase "" (assertFailure (show e))
              Right f' -> testGroup "" $ validate f'
  where
    validate :: Frame -> [Test]
    validate (Frame s d _ b) =
      [
        testCase "" $ assertMaybeEqual "src" (srcCallsign =<< result f) s,
        testCase "" $ assertMaybeEqual "dst" (dstCallsign =<< result f) d,
        testCase "" $ assertMaybeEqual "body" (FAPTests.body =<< result f) b
      ]
    assertMaybeEqual lbl a b = assertEqual lbl (fromJust a) (show b)

tests :: IO [Test]
tests = do
  jstr <- B.readFile "test/faptests.json"
  let tj = case eitherDecode jstr :: Either String [FAPTest] of
             Right x -> x
             Left x -> error ("decoding junk: " ++ show x)

  return $ map fapTest tj
