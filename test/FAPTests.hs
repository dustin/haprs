{-# LANGUAGE OverloadedStrings #-}

module FAPTests (tests) where

import APRS.Types

import Control.Monad (foldM)
import Data.Aeson
import Data.Maybe
import Text.Read (readEither)
import qualified Data.ByteString.Lazy as B

import Test.Tasty
import Test.Tasty.HUnit

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

fapTest :: [FAPTest] -> TestTree
fapTest fs = let parsed = map (\f -> case readEither (src f) :: Either String Frame of
                                  Left e -> error (show e)
                                  Right f' -> (f,f')) fs in
               testCaseInfo "FAP tests" $ do
                 asses <- foldM (\n (f, (Frame s d _ b)) -> do
                                    assertMaybeEqual "src" f srcCallsign s
                                    assertMaybeEqual "dst" f dstCallsign d
                                    assertMaybeEqual "body" f FAPTests.body b
                                    return $ n + 3
                                ) (0::Int) parsed
                 return $ show asses ++ " assertions run"
  where assertMaybeEqual lbl f a b = assertEqual lbl (fromJust (a =<< result f)) (show b)

tests :: IO TestTree
tests = do
  jstr <- B.readFile "test/faptests.json"
  let tj = case eitherDecode jstr :: Either String [FAPTest] of
             Right x -> x
             Left x -> error ("decoding junk: " ++ show x)

  return $ fapTest $ filter (\(FAPTest _ _ n) -> n == 0) tj
