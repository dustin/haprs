{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module APRS.Types
    ( PacketType
    , Address
    , address
    , Similar
    , (≈)
    , Frame(..)
    , Body(..)
    , Position(..)
    , Velocity(..)
    , Message(..)
    , message
    , position
    , identifyPacket
    , callPass
    , decodeBase91
    -- parsers
    , parseAddr
    , parsePosition
    , parsePosUncompressed
    , parsePosCompressed
    , parseFrame
    -- For testing
    ) where

import Prelude hiding (any, take, drop, head, takeWhile)
import Control.Applicative ((<|>))
import Control.Monad (replicateM)
import Data.Either (either)
import Data.String (fromString)
import Data.Text (Text, any, take, drop, head, uncons, dropAround, splitOn,
                  length, intercalate, unpack, concat)
import Data.Bits (xor, (.&.), shiftL)
import Data.Int (Int16)
import Text.Read (readMaybe)
import qualified Data.Attoparsec.Text as A

class Similar a where
  (≈) :: a -> a -> Bool

data PacketType = CurrentMicEBin
  | CurrentMicE
  | Item
  | PositionNoTSNoMsg
  | PositionTSNoMsg
  | MessagePkt
  | Object
  | StationCaps
  | PositionNoTS
  | PositionNoMsg
  | PositionMsg
  | Status
  | Query
  | Telemetry
  | WeatherNoPos
  | UserDefined
  | ThirdParty
  | InvalidPacket Char
  deriving (Show, Ord, Eq)

identifyPacket :: Char -> PacketType
identifyPacket '\x1c' = CurrentMicEBin
identifyPacket '!' = PositionNoTSNoMsg
identifyPacket ')' = Item
identifyPacket '/' = PositionNoMsg
identifyPacket ':' = MessagePkt
identifyPacket ';' = Object
identifyPacket '<' = StationCaps
identifyPacket '=' = PositionNoTS
identifyPacket '>' = Status
identifyPacket '?' = Query
identifyPacket '@' = PositionMsg
identifyPacket 'T' = Telemetry
identifyPacket '_' = WeatherNoPos
identifyPacket '`' = CurrentMicE
identifyPacket '{' = UserDefined
identifyPacket '}' = ThirdParty
identifyPacket x = InvalidPacket x

-- Address Callsign SSID
data Address = Address Text Text deriving (Eq)

addrChars :: [Char]
addrChars = ['A'..'Z'] ++ ['0'..'9']

address :: Text -> Text -> Either String Address
address c s
  | Data.Text.length c < 1 = Left "callsign is too short"
  | Data.Text.length c > 12 = Left "callsign is too long"
  | Data.Text.length s > 6 = Left "SSID is too long"
  | invalid c = Left "invalid characters in callsign"
  | invalid s = Left "invalid characters in SSID"
  | otherwise = Right $ Address c s
  where invalid :: Text -> Bool
        invalid = any (`notElem` addrChars)

instance Show Address where
  show (Address c "") = unpack c
  show (Address c s) = unpack c ++ "-" ++ unpack s

parseAddr :: A.Parser Address
parseAddr = do
  c <- A.takeWhile (A.inClass addrChars)
  ss <- (A.string "-" >> A.takeWhile (A.inClass $ fromString addrChars)) <|> A.string ""
  either fail return $ address c ss

instance Read Address where
  readsPrec _ x = either error (\a -> [(a,"")]) $ A.parseOnly parseAddr (fromString x)

ctoi :: Char -> Int16
ctoi = toEnum . fromEnum

callPass :: Address -> Int16
callPass (Address a _) =
  0x7fff .&. foldl xor 0x73e2 (map (\(c, f) -> f (ctoi c)) $ zip a' $ cycle [flip shiftL 8, id])
  where a' = unpack a

instance Similar Address where
  (≈) (Address a _) (Address b _) = a == b

newtype Body = Body Text deriving (Eq)

instance Show Body where show (Body x) = unpack x

pktType :: Body -> Maybe PacketType
pktType (Body b) = identifyPacket . fst <$> uncons b

b91chars :: String
b91chars = "[!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\^_`abcdefghijklmnopqrstuvwxyz{]"

parseB91Seg :: A.Parser Double
parseB91Seg = do
  stuff <- replicateM 4 (A.satisfy (`elem` b91chars))
  return $ (fromIntegral.decodeBase91.fromString) stuff

posamb :: Int -> Double
posamb 0 = 0
posamb 1 = 0.05 / 60
posamb 2 = 0.5 / 60
posamb 3 = 5 / 60
posamb 4 = 0.5
posamb _ = error "Invalid ambiguity"

parsePosition :: A.Parser Position
parsePosition = parsePosCompressed <|> parsePosUncompressed

parsePosUncompressed :: A.Parser Position
parsePosUncompressed = do
  -- _ts <- poshdr <|> timestamphdr
  lat <- parseDir "lat " 2
  _sym <- A.satisfy (A.inClass "0-9/\\A-z") A.<?> "lat/lon separator"
  lon <- parseDir "lon " 3
  v <- A.eitherP pvel (A.string "")

  return $ Position (lat,lon, either Just (const Nothing) v)

  where
    parseDir :: String -> Int -> A.Parser Double
    parseDir lbl n = do
      cM <- replicateM n A.digit A.<?> (lbl ++ "first digits")
      cm <- A.many' A.digit A.<?> (lbl ++ "second digits")
      cs1 <- A.many' A.space A.<?> (lbl ++ "first optional spaces")
      _ <- A.string "." A.<?> (lbl ++ "decimal")
      cd <- A.many' A.digit A.<?> (lbl ++ "decimal digits")
      cs2 <- A.many' A.space A.<?> (lbl ++ "second optional space")
      cdir <- A.satisfy (`elem` ['N', 'S', 'E', 'W']) A.<?> (lbl ++ "direction")

      let amb = Prelude.length cs1 + Prelude.length cs2
      return $ (compPos cM (cm ++ cs1) (cd ++ cs2) + posamb amb) * psign cdir

    psign 'S' = -1
    psign 'W' = -1
    psign _ = 1

    poshdr = A.satisfy (`elem` ['!', '=']) >> pure ""
    timestamphdr = do
      _p <- A.satisfy (`elem` ['/', ',', '@', '\\', '*'])
      ts <- replicateM 6 A.digit
      _ <- A.satisfy (`elem` ['h', 'z', '/'])
      return ts

    compPos :: String -> String -> String -> Double
    compPos a b c = let a' = (rz . Prelude.concat) [replspc a]
                        b' = (rz . Prelude.concat) [replspc b, ".", replspc c]
                    in a' + (b' / 60)
    replspc = map (\c -> if c == ' ' then '0' else c)
    rz = read :: String -> Double

    pvel :: A.Parser Velocity
    pvel = do
      _ <- A.anyChar
      d1 <- A.satisfy (`elem` ['0', '1', '2', '3'])
      d2 <- replicateM 2 A.digit
      _ <- A.string "/"
      d3 <- replicateM 3 A.digit

      let a = read (d1:d2) :: Double
      let b = (* 1.852) $ read d3 :: Double
      return $ Velocity (a, b)

parsePosCompressed :: A.Parser Position
parsePosCompressed = do
  _symbol <- A.satisfy (`elem` ['!', '=', '/', '@'])
  b91a <- parseB91Seg
  b91b <- parseB91Seg
  _ <- A.anyChar
  vel <- replicateM 2 A.anyChar
  _ <- A.anyChar

  return $ Position $ unc b91a b91b (fmap fromEnum vel)

  where
    unc m1 m2 m5 = (90 - (m1 / 380926), (-180) + (m2 / 190463), pcvel m5)
    pcvel [a,b]
      | a >= 33 && a <= 122 = let course = fromIntegral (a - 33) * 4
                                  speed = 1.852 * ((1.08 ^ (b - 33)) - 1)
                                  course' = if course == 0 then 360 else course in
                                Just $ Velocity (course', speed)
    pcvel _ = Nothing

newtype Velocity = Velocity (Double, Double) deriving (Eq)

instance Show Velocity where
  show (Velocity (course, speed)) = show speed ++ " kph @" ++ (show.round) course ++ "°"

-- data Position = Position { _pos :: Geodetic WGS84, _ambiguity :: Int }
-- lon, lat, velocity
newtype Position = Position (Double, Double, Maybe Velocity) deriving (Eq, Show)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

position :: Body -> Maybe Position
position bod@(Body bt)
  | Data.Text.length bt < 6 = Nothing
  | otherwise = go $ pktType bod
  where go Nothing = Nothing
        go (Just t)
          | t `elem` [PositionNoTSNoMsg, PositionNoTS]  = parse (drop 1 bt)
          | t `elem` [PositionNoMsg, PositionMsg]       = parse (drop 8 bt)
          | t == Object                                 = parse (drop 19 bt)
          | otherwise                                   = parse bt
        parse = eitherToMaybe . A.parseOnly parsePosition

subt' :: Int -> Int -> Text -> Text
subt' s n = take n.drop s

data Message = Message { msgSender :: Address
                       , msgRecipient :: Address
                       , msgBody :: Text
                       , msgID :: Text
                       }

message :: Frame -> Maybe Message
message (Frame s _ _ (Body b))
  | Data.Text.length b < 12 = Nothing
  | head b /= ':' = Nothing
  | otherwise = let rc = readMaybe $ unpack ((dropAround (== ' ').subt' 1 9) b) :: Maybe Address
                    (bod:rest) = splitOn "{" (drop 11 b) in
                  rc >>= \r -> pure $ Message s r bod (Data.Text.concat rest)

-- Source Dest Path Body
data Frame = Frame Address Address [Text] Body
           deriving (Eq)

parseFrame :: A.Parser Frame
parseFrame = do
  src <- parseAddr
  _ <- A.string ">"
  dest <- parseAddr
  _ <- A.string "," <|> A.string "" -- maybe comma
  path <- A.sepBy (A.takeWhile (A.notInClass ",:")) (A.char ',')
  _ <- A.string ":"
  bod <- A.takeText
  return $ Frame src dest path (Body bod)

instance Read Frame where
  readsPrec _ x = either error (\f -> [(f,"")]) $ A.parseOnly parseFrame (fromString x)

instance Show Frame where
  show (Frame s d p b) =
    show s ++ ">" ++ show d ++ "," ++ (unpack.intercalate ",") p ++ ":" ++ show b

decodeBase91 :: String -> Int
decodeBase91 s@[_,_,_,_] =
  foldl (\a (c, i) -> i * ((toEnum . fromEnum $ c) -33) + a) 0 $ zip s [91^x | x <- [3,2..0]]
decodeBase91 _ = 0
