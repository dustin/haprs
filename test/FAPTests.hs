{-# LANGUAGE OverloadedStrings #-}

module FAPTests (tests) where

import APRS.Types

import Control.Exception (catch, SomeException, evaluate)
import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.Aeson
import Data.Maybe
import Text.Read (readEither)
import qualified Data.ByteString.Lazy as B

import Test.Tasty
import Test.Tasty.HUnit
import Test.HUnit.Approx (assertApproxEqual)

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
  , latitude :: Maybe Double
  , longitude :: Maybe Double
  , _posResolution :: Maybe Double
  , _posAmbiguity :: Maybe Double
  , body :: Maybe String
  , format :: Maybe String
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

ε :: Double
ε = 0.001

fapTest :: [FAPTest] -> TestTree
fapTest fs = let parsed = map (\f -> case readEither (src f) :: Either String Frame of
                                  Left e -> error (show e)
                                  Right f' -> (f,f')) fs in
               testCaseInfo "FAP tests" $ do
                 asses <- foldM (\n (f, (Frame s d _ b)) -> do
                                    assertMaybeEqual "src" f srcCallsign s
                                    assertMaybeEqual "dst" f dstCallsign d
                                    assertMaybeEqual "body" f FAPTests.body b

                                    let res = (fromJust.result) f
                                    pos <- catch (evaluate $ position b) (\e -> do
                                                                             let _ = (e :: SomeException)
                                                                             return Nothing)
                                    let (Just (Position (plat, plon))) = pos <|> Just (Position (0, 0))
                                    let wantpos = isJust $ latitude res

                                    pn <- if not (isJust pos && wantpos) then return 0 else do
                                      let elat = if format res == Just "uncompressed" then 0
                                                 else (fromMaybe 0.latitude) res
                                      let elon = if format res == Just "uncompressed" then 0
                                                 else (fromMaybe 0.longitude) res
                                      assertApproxEqual ("lat " ++ show b) ε elat plat
                                      assertApproxEqual ("lon " ++ show b) ε elon plon

                                      return 2
                                    return $ n + 3 + pn
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
