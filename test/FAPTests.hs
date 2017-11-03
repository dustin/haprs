{-# LANGUAGE OverloadedStrings #-}

module FAPTests (tests) where

import APRS.Types

import Control.Exception (catch, SomeException, evaluate)
import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.Aeson
import Data.Maybe
import Data.Text (unpack)
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
  , _format :: Maybe String
  , _messaging :: Maybe Int
  , _origPacket :: Maybe String
  , _typ :: Maybe String
  , _header :: Maybe String
  , _digipeaters :: Maybe [Digipeater]
  , speed :: Maybe Double
  , course :: Maybe Double
  , fapmsg :: Maybe String
  , fapmsgid :: Maybe String
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
    <*> v .:? "speed"
    <*> v .:? "course"
    <*> v .:? "message"
    <*> v .:? "messageid"

data FAPTest = FAPTest {
  src :: !String
  , result :: Maybe FAPResult
  , _failed :: Int
  , _misunderstood :: Bool
  } deriving (Show)

instance FromJSON FAPTest where
    parseJSON = withObject "FAPTest" $ \v -> FAPTest
        <$> v .: "src"
        <*> v .:? "result"
        <*> v .:? "failed" .!= 0
        <*> v .:? "misunderstood" .!= False

ε :: Double
ε = 0.001

fapTest :: [FAPTest] -> TestTree
fapTest fs = let parsed = map (\f -> case readEither (src f) :: Either String Frame of
                                  Left e -> error (show e)
                                  Right f' -> (f,f')) fs in
               testCaseInfo "FAP tests" $ do
                 asses <- foldM (\n (f, frame@(Frame s d _ b)) -> do
                                    assertMaybeEqual "src" f srcCallsign s
                                    assertMaybeEqual "dst" f dstCallsign d
                                    assertMaybeEqual "body" f FAPTests.body b

                                    let res = (fromJust.result) f
                                    pos <- catch (evaluate $ position b) (\e -> do
                                                                             let _ = (e :: SomeException)
                                                                             return Nothing)
                                    let (Just (Position (plat, plon, vel))) = pos <|> Just (Position (0, 0, Nothing))
                                    let wantpos = isJust $ latitude res

                                    pn <- if not (isJust pos && wantpos) then return 0 else do
                                      let elat = (fromMaybe 0.latitude) res
                                      let elon = (fromMaybe 0.longitude) res
                                      assertApproxEqual ("lat " ++ show b) ε elat plat
                                      assertApproxEqual ("lon " ++ show b) ε elon plon

                                      let wantvel = isJust $ speed res
                                      vn <- if not (isJust vel && wantvel) then return 0 else do
                                        let (Velocity (crs, spd)) = fromJust vel
                                        let ecrs = (fromMaybe 0.course) res
                                        let espd = (fromMaybe 0.speed) res
                                        assertApproxEqual ("course " ++ show b) ε ecrs crs
                                        assertApproxEqual ("speed " ++ show b) ε espd spd

                                        return 2

                                      return (2 + vn)

                                    let wantmsg = isJust $ fapmsg res
                                    let msg = message frame
                                    mn <- if not (isJust msg && wantmsg) then return 0 else do
                                      let (Message sndr _ bod msgid) = fromJust msg
                                      assertMaybeEqual ("msg sender: " ++ show b) f srcCallsign sndr
                                      assertEqual ("msg bod: " ++ show b) (fromJust . fapmsg $ res) (unpack bod)
                                      assertEqual ("msgid: " ++ show b) (fromJust . fapmsgid $ res) (unpack msgid)

                                      return 3

                                    return $ n + 3 + pn + mn
                                ) (0::Int) parsed
                 return $ show asses ++ " assertions run"
  where assertMaybeEqual lbl f a b = assertEqual lbl (fromJust (a =<< result f)) (show b)

tests :: IO TestTree
tests = do
  jstr <- B.readFile "test/faptests.json"
  let tj = case eitherDecode jstr :: Either String [FAPTest] of
             Right x -> x
             Left x -> error ("decoding junk: " ++ show x)

  return $ fapTest $ filter (\(FAPTest _ _ n m) -> (n == 0 && m == False)) tj
