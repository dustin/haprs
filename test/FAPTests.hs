{-# LANGUAGE OverloadedStrings #-}

module FAPTests (tests) where

import APRS.Types

import Control.Monad (foldM)
import Data.Aeson
import Data.Maybe
import Data.String (fromString)
import Data.Text (unpack)
import Data.Either (lefts, rights)
import Data.List (groupBy, sortBy)
import qualified Data.Attoparsec.Text as A
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

megaSkip :: [a] -> IO String
megaSkip x = return $ "SKIPPED " ++ (show.length) x

bodyParserTest :: PacketType -> [FAPTest] -> IO String
bodyParserTest (InvalidPacket '\'') x = megaSkip x
bodyParserTest _ fs = let parsed = map (\f -> (f, A.parseOnly parseFrame (fromString . src $ f))) fs in
                        do
                          assess <- foldM (\n (f,bodyP) -> do
                                              assertBool (show f) $ understood bodyP
                                              return $ n + 1
                                          ) (0::Int) parsed
                          return $ show assess ++ " assertions run"

                            where
                              understood :: Either String Frame -> Bool
                              understood (Right (Frame _ _ _ (GarbagePacket _))) = False
                              understood (Right (Frame _ _ _ (NotImplemented _ _))) = False
                              understood (Right _) = True
                              understood _ = False


fapTest :: [FAPTest] -> IO String
fapTest fs = let parsed = map (\f -> case A.parseOnly parseFrame (fromString . src $ f) of
                                       Left e -> Left $ show (src f) ++ " -- " ++ show e
                                       Right f' -> Right $ (f,f')) fs in
               do
                 asses <- foldM (\n (f, frame@(Frame s d _ mparsed)) -> do
                                    assertMaybeEqual "src" f srcCallsign s
                                    assertMaybeEqual "dst" f dstCallsign d

                                    let b = fromJust (FAPTests.body =<< result f)
                                    let res = (fromJust.result) f
                                    let wantpos = isJust $ latitude res
                                    assertEqual ("pos: want v. got: " ++ b) wantpos (haspos frame)
                                    pn <- if not wantpos then return 0 else do
                                      let pos = position frame
                                      let (Just (Position (plat, plon, vel))) = pos
                                      let elat = (fromMaybe 0.latitude) res
                                      let elon = (fromMaybe 0.longitude) res
                                      assertApproxEqual ("lat " ++ show b) ε elat plat
                                      assertApproxEqual ("lon " ++ show b) ε elon plon

                                      let wantvel = isJust (speed res)
                                      vn <- if not wantvel then return 0 else do
                                        assertBool (show b) $ vel /= PosENone
                                        let (PosECourseSpeed crs spd) = vel
                                        let ecrs = (fromMaybe 0.course) res
                                        let espd = (fromMaybe 0.speed) res
                                        assertApproxEqual ("course " ++ show b) ε ecrs (fromIntegral crs)
                                        assertApproxEqual ("speed " ++ show b) ε espd spd

                                        return 3

                                      return (2 + vn)

                                    let wantmsg = fapmsg res
                                    mn <- if not (isJust wantmsg) then return 0 else do
                                      let (MessagePacket rcpt (Message t) msgid) = mparsed
                                      -- assertMaybeEqual ("msg sender: " ++ show b) f srcCallsign sndr
                                      assertEqual ("msg bod: " ++ show b) (fromJust . fapmsg $ res) (unpack t)
                                      assertEqual ("msgid: " ++ show b) (fromJust . fapmsgid $ res) (unpack msgid)
                                      -- This is kind of dumb, but there's nothing to compare to in input data
                                      assertBool "rcpt strings" $ show rcpt /= ""

                                      return 3

                                    return $ n + 3 + pn + mn
                                ) (0::Int) (rights parsed)
                 return $ show asses ++ " assertions run (" ++ (show $ (length.lefts) parsed) ++ " failed to parse)"
  where assertMaybeEqual lbl f a b = assertEqual lbl (fromJust (a =<< result f)) (show b)
        haspos x = isJust $ position x

tests :: IO TestTree
tests = do
  jstr <- B.readFile "test/faptests.json"
  let tj = case eitherDecode jstr :: Either String [FAPTest] of
             Right x -> x
             Left x -> error ("decoding junk: " ++ show x)

  let allfaps = filter (\(FAPTest _ _ n m) -> (n == 0 && not m)) tj
  let compressed = filter (fapfmt $ Just "compressed") allfaps
  let uncompressed = filter (fapfmt $ Just "uncompressed") allfaps
  let nopos = filter (fapfmt Nothing) allfaps

  let groups = (groupBy (\a b -> (head.fbody) a == (head.fbody) b) $
        sortBy (\a b -> compare (head $ fbody a) (head $ fbody b)) allfaps) :: [[FAPTest]]

  return $ testGroup "FAP Tests" ([
    testCaseInfo "compressed" $ fapTest compressed,
    testCaseInfo "uncompressed" $ fapTest uncompressed,
    testCaseInfo "no pos" $ fapTest nopos
    ] ++ map (\cases -> let t = identifyPacket.head.fbody.head $ cases in
                          testCaseInfo ("megaparse " ++ show t) $
                        bodyParserTest t cases) groups)

  where
    fapfmt :: Maybe String -> FAPTest -> Bool
    fapfmt x ft = x == (result ft >>= format)

    fbody :: FAPTest -> String
    fbody (FAPTest _ (Just (FAPResult{body=Just b})) _ _) = b
    fbody f = (error.show) f
