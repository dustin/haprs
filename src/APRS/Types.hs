{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module APRS.Types
    ( PacketType(..)
    , Address
    , address
    , Similar
    , (≈)
    , Frame(..)
    , Body(..)
    , Position(..)
    , Velocity(..)
    , Message(..)
    , Timestamp(..)
    , WeatherParam(..)
    , message
    , position
    , identifyPacket
    , callPass
    , decodeBase91
    -- New parser
    , APRSPacket(..)
    , PosExtension(..)
    , Symbol(..)
    , Directivity(..)
    , MessageInfo(..)
    -- parsers
    , parseAddr
    , parsePosition
    , parsePosUncompressed
    , parsePosCompressed
    , parseFrame
    , parseTimestamp
    , parseWeather
    , parseMessage
    , megaParser
    , findParse
    -- For testing
    ) where

import Control.Applicative ((<|>))
import Control.Monad (replicateM, replicateM_, guard)
import Data.Bits (xor, (.&.), shiftL)
import Data.Char (digitToInt)
import Data.Either (either, rights)
import Data.Int (Int16)
import Data.Word (Word8)
import Data.String (fromString)
import Data.Text (Text, any, all, length, intercalate, unpack, tails)
import Numeric (readInt)
import Prelude hiding (any)
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
  c <- A.takeWhile (A.inClass "A-Z0-9")
  ss <- (A.string "-" >> A.takeWhile (A.inClass "A-Z0-9")) <|> pure ""
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
parsePosition = parsePosUncompressed <|> parsePosCompressed

parsePosUncompressed :: A.Parser Position
parsePosUncompressed = do
  _ts <- poshdr <|> timestamphdr
  lat <- parseDir "lat " 2
  _sym <- A.satisfy (A.inClass "0-9/\\A-z") A.<?> "lat/lon separator"
  lon <- parseDir "lon " 3
  v <- A.eitherP pvel (pure "")

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

    poshdr = A.satisfy (`elem` ['!', '=']) >> pure Timeless
    timestamphdr = do
      _p <- A.satisfy (`elem` ['/', ',', '@', '\\', '*'])
      parseTimestamp

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

data Timestamp = DHMLocal (Int, Int, Int)
               | DHMZulu (Int, Int, Int)
               | HMS (Int, Int, Int)
               | MDHM (Int, Int, Int, Int)
               | Timeless
  deriving (Show, Eq)

parseTimestamp :: A.Parser Timestamp
parseTimestamp = dhmlocal <|> dhmzulu <|> hms <|> mdhm
  where
    dhmlocal = DHMLocal <$> n3 '/'
    dhmzulu = DHMZulu <$> n3 'z'
    hms = HMS <$> n3 'h'
    mdhm = n 4 >>= \[m, d, h, mn] -> pure $ MDHM (m, d, h, mn)

    n :: Int -> A.Parser [Int]
    n x = replicateM x (replicateM 2 A.digit) >>= \digs -> return $ map read digs
    n3 :: Char -> A.Parser (Int, Int, Int)
    n3 ch = n 3 >>= \[a,b,c] -> A.char ch >> pure (a,b,c)

parsePosCompressed :: A.Parser Position
parsePosCompressed = do
  _ts <- timething <|> plainpos <|> obj
  _sym <- A.anyChar
  b91a <- parseB91Seg A.<?> "first b91 seg"
  b91b <- parseB91Seg A.<?> "second b91 seg"
  _ <- A.anyChar -- symbol code
  vel <- replicateM 2 A.anyChar
  _ <- A.anyChar -- compression type

  return $ Position $ unc b91a b91b (fmap fromEnum vel)

  where
    unc m1 m2 m5 = (90 - (m1 / 380926), (-180) + (m2 / 190463), pcvel m5)
    pcvel [a,b]
      | a >= 33 && a <= 122 = let course = fromIntegral (a - 33) * 4
                                  speed = 1.852 * ((1.08 ^ (b - 33)) - 1)
                                  course' = if course == 0 then 360 else course in
                                Just $ Velocity (course', speed)
    pcvel _ = Nothing
    timething :: A.Parser Timestamp
    timething = do
      _ptyp <- A.satisfy (`elem` ['/', '@'])
      parseTimestamp
    plainpos :: A.Parser Timestamp
    plainpos = A.satisfy (`elem` ['!', '=']) >> pure Timeless
    obj :: A.Parser Timestamp
    obj = do
      _ptyp <- A.satisfy (== ';')
      _objname <- replicateM 9 A.anyChar
      _objstate <- A.satisfy (`elem` ['_', '*']) -- killed, live
      parseTimestamp

newtype Velocity = Velocity (Double, Double) deriving (Eq)

instance Show Velocity where
  show (Velocity (course, speed)) = show speed ++ " kph @" ++ (show.round) course ++ "°"

-- data Position = Position { _pos :: Geodetic WGS84, _ambiguity :: Int }
-- lon, lat, velocity
newtype Position = Position (Double, Double, Maybe Velocity) deriving (Eq, Show)

position :: Body -> Maybe Position
position (Body bt)
  | Data.Text.length bt < 6 = Nothing
  | otherwise = findParse parsePosition bt

data Message = Message { msgSender :: Address
                       , msgRecipient :: Address
                       , msgBody :: Text
                       , msgID :: Text
                       }
               deriving (Show)

findParse :: A.Parser a -> Text -> Maybe a
findParse p s = case rights $ map (A.parseOnly p) $ Data.Text.tails s of
                  [] -> Nothing
                  (x:_) -> Just x

parseMessage :: Address -> A.Parser Message
parseMessage s = do
  _ <- A.char ':' -- message indicator
  rcpt <- parseAddr
  _ <- A.many' A.space -- Technically, rpct is 9 characters with trailing space, but we're Posteling here a bit.
  _ <- A.char ':' -- message separator
  mtext <- A.many' (A.satisfy (`notElem` ['{', '|', '~']))
  mid <- ("{" *> A.takeText) <|> pure "" -- message ID is optional

  return $ Message s rcpt (fromString mtext) mid

message :: Frame -> Maybe Message
message (Frame s _ _ (Body b)) = either (const Nothing) Just $ A.parseOnly (parseMessage s) b

data WeatherParam = WindDir Int
                  | WindSpeed Int
                  | WindGust Int
                  | Temp Int
                  | RainLastHour Int
                  | RainLast24Hours Int
                  | RainToday Int
                  | Humidity Int
                  | Baro Int
                  | NoData Char
                  deriving (Show, Eq)

parseWParam :: A.Parser WeatherParam
parseWParam = w 'c' WindDir
              <|> w 's' WindSpeed
              <|> w 'g' WindGust
              <|> w 't' Temp
              <|> w 'r' RainLastHour
              <|> w 'p' RainLast24Hours
              <|> w 'P' RainToday
              <|> w' 2 'h' Humidity
              <|> w' 5 'b' Baro
  where
    w' :: Int -> Char -> (Int -> WeatherParam) -> A.Parser WeatherParam
    w' i c wc = do
      _ <- A.char c
      wgood i wc <|> wnodata i c

    wgood i wc = replicateM i A.digit >>= \deez -> return $ wc (read deez)
    wnodata i c = replicateM_ i (A.satisfy (`elem` ['.', ' '])) >> return (NoData c)

    w = w' 3

parseWeather :: A.Parser [WeatherParam]
parseWeather = A.many1 parseWParam

-- Source Dest Path Body
data Frame = Frame Address Address [Text] Body
           deriving (Eq)

parseFrame :: A.Parser Frame
parseFrame = do
  src <- parseAddr
  _ <- A.string ">"
  dest <- parseAddr
  _ <- A.string "," <|> pure "" -- maybe comma
  path <- A.sepBy (A.takeWhile (`notElem` [',', ':'])) (A.char ',')
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

{-
• Position
• Direction Finding
• Objects and Items
• Weather
• Telemetry
• Messages, Bulletins and Announcements
• Queries
• Responses
• Status
• Other
-}

data PosExtension = PosECourseSpeed Int Int
                  | PosEPHG Int Int Int Directivity
                  | PosERNG
                  | PosEDFS
                  | PosETypeDesc
                  | PosENone
                  deriving (Show, Eq)

data Directivity = Omni | DirNE | DirE | DirSE | DirS | DirSW | DirW | DirNW | DirN
                 deriving (Show, Eq, Ord, Enum, Bounded)

parsePosExtension :: A.Parser PosExtension
parsePosExtension = parseCrsSpd
                    <|> parsePHG
                    <|> pure PosENone

  where
    parseCrsSpd = do
      crs <- replicateM 3 A.digit
      _ <- A.char '/'
      spd <- replicateM 3 A.digit
      let c = read crs :: Int
      let s = read spd :: Int
      guard $ c > 0 && c < 361
      return $ PosECourseSpeed (if c == 360 then 0 else c) (ceiling $ fromIntegral s * 1.852)

    parsePHG = do
      _ <- A.string "PHG"
      p <- A.digit
      h <- A.digit
      g <- A.digit
      d <- A.satisfy (A.inClass "0-8")

      let d' = (toEnum . digitToInt) d :: Directivity

      return $ PosEPHG (digitToInt p ^ 2) (10 * 2 ^ digitToInt h) (digitToInt g) d'

data Symbol = Symbol Char Char deriving (Show, Eq)

data MessageInfo = Message' Text
                 | MessageACK
                 | MessageNAK
                   deriving (Show, Eq)

-- TODO:  Include extensions from page 27 in position packets
data APRSPacket = PositionPacket PacketType Symbol (Double, Double) (Maybe Timestamp) PosExtension Text
                | ObjectPacket Symbol Text (Double, Double) Timestamp Text
                | ItemPacket Symbol Text (Double, Double) Text
                | WeatherPacket (Maybe Timestamp) (Maybe (Double, Double)) [WeatherParam] Text
                | StatusPacket (Maybe Timestamp) Text
                | MessagePacket Address MessageInfo Text -- includes sequence number
                | TelemetryPacket Text [Double] Word8 Text -- seq, vals, bits, comment
                deriving (Show, Eq)

megaParser :: A.Parser APRSPacket
megaParser = parseWeatherPacket
             <|> parsePositionPacket
             <|> parseObjectPacket
             <|> parseItemPacket
             <|> parseStatusPacket
             <|> parseMessagePacket
             <|> parseTelemetry

{-
|       | No MSG | MSG |
| NO TS | !      | =   |
| TS    | /      | @   |
-}

parsePosition' :: Char -> A.Parser (Symbol, Position)
parsePosition' tbl = parsePosUncompressed' <|> parsePosCompressed' tbl

parsePosCompressed' :: Char -> A.Parser (Symbol, Position)
parsePosCompressed' tbl = do
  b91a <- parseB91Seg A.<?> "first b91 seg"
  b91b <- parseB91Seg A.<?> "second b91 seg"
  sym <- A.anyChar -- symbol code
  _vel <- replicateM 2 A.anyChar
  _ <- A.anyChar -- compression type

  return (Symbol tbl sym, Position $ unc b91a b91b)

  where
    unc m1 m2 = (90 - (m1 / 380926), (-180) + (m2 / 190463), Nothing)

parsePosUncompressed' :: A.Parser (Symbol, Position)
parsePosUncompressed' = do
  lat <- parseDir "lat " 2
  tbl <- A.satisfy (A.inClass "0-9/\\A-Za-j") A.<?> "lat/lon separator (tbl)"
  lon <- parseDir "lon " 3
  sym <- A.anyChar A.<?> "lat/lon separator (sym)"

  return (Symbol tbl sym, Position (lat,lon, Nothing))

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

    compPos :: String -> String -> String -> Double
    compPos a b c = let a' = (rz . Prelude.concat) [replspc a]
                        b' = (rz . Prelude.concat) [replspc b, ".", replspc c]
                    in a' + (b' / 60)
    replspc = map (\c -> if c == ' ' then '0' else c)
    rz = read :: String -> Double


parsePositionPacket :: A.Parser APRSPacket
parsePositionPacket = do
  pre <- A.satisfy (`elem` ['!', '=', '/', '@'])
  ts <- maybeTS pre
  (sym, Position (lat,lon,_)) <- parsePosition' pre
  posE <- parsePosExtension
  com <- A.takeText
  return $ PositionPacket (identifyPacket pre) sym (lat,lon) ts posE com

  where
    maybeTS :: Char -> A.Parser (Maybe Timestamp)
    maybeTS '!' = pure Nothing
    maybeTS '=' = pure Nothing
    maybeTS _ = Just <$> parseTimestamp

parseObjectPacket :: A.Parser APRSPacket
parseObjectPacket = do
  _ <- A.satisfy (== ';')
  name <- replicateM 9 A.anyChar
  _objstate <- A.satisfy (`elem` ['_', '*']) -- killed, live
  ts <- parseTimestamp
  (sym, Position (lat,lon,_)) <- parsePosition' ';'
  comment <- A.takeText
  return $ ObjectPacket sym (fromString name) (lat, lon) ts comment

parseItemPacket :: A.Parser APRSPacket
parseItemPacket = do
  _ <- A.satisfy (== ')')
  name <- A.takeTill (\c -> c == '_' || c == '!')
  guard $ Data.Text.length name >= 3 && Data.Text.length name <= 9
  _objstate <- A.satisfy (`elem` ['_', '!']) -- killed, live
  (sym, Position (lat,lon,_)) <- parsePosition' ')'
  comment <- A.takeText
  return $ ItemPacket sym name (lat, lon) comment

-- Examples that aren't recognized properly:
--   @092345z/5L!!<*e7 _7P[g005t077r000p000P000h50b09900wRSW
parseWeatherPacket :: A.Parser APRSPacket
parseWeatherPacket = do
  c <- A.satisfy (`elem` ['_', '/', '!', '@', '='])
  ts <- (parseTimestamp >>= \t -> return (Just t)) <|> pure Nothing
  pos <- if c `elem` ['_', '='] then pure Nothing else ppos
  _w <- wind <|> pure (0,0)
  wp <- parseWeather
  rest <- A.takeText
  return $ WeatherPacket ts pos wp rest

  where
    ppos :: A.Parser (Maybe (Double, Double))
    ppos = do
      (_, Position (lat,lon,_)) <- parsePosition' 'x'
      return $ Just (lat,lon)

    wind :: A.Parser (Int, Int)
    wind = do
      wspd <- replicateM 3 A.digit
      _ <- A.char '/'
      wdir <- replicateM 3 A.digit
      return (read wspd, read wdir)

parseStatusPacket :: A.Parser APRSPacket
parseStatusPacket = do
  _ <- A.char '>'
  ts <- (parseTimestamp >>= \t -> return (Just t)) <|> pure Nothing
  msg <- A.many1 (A.satisfy (`notElem` ['|', '~']))
  return $ StatusPacket ts (fromString msg)

parseMessagePacket :: A.Parser APRSPacket
parseMessagePacket = do
  _ <- A.char ':' -- message indicator
  rcpt <- parseAddr
  _ <- A.many' A.space -- Technically, rpct is 9 characters with trailing space, but we're Posteling here a bit.
  _ <- A.char ':' -- message separator
  (mi, mid) <- parseMI

  return $ MessagePacket rcpt mi mid

  where
    parseMI :: A.Parser (MessageInfo, Text) -- returns the message ID
    parseMI = parseACKNAK "ack" MessageACK <|> parseACKNAK "rej" MessageNAK <|> parseMsg

    parseACKNAK :: Text -> MessageInfo -> A.Parser (MessageInfo, Text)
    parseACKNAK pre i = do
      _ <- A.string pre
      msgid <- A.takeText
      guard $ msgid /= ""
      guard $ Data.Text.length msgid <= 5
      guard $ Data.Text.all (A.inClass "A-z0-9") msgid
      return (i, msgid)

    parseMsg :: A.Parser (MessageInfo, Text)
    parseMsg = do
      mtext <- A.many' (A.satisfy (`notElem` ['{', '|', '~']))
      mid <- ("{" *> A.takeText) <|> pure "" -- message ID is optional
      return (Message' (fromString mtext), mid)

parseTelemetry :: A.Parser APRSPacket
parseTelemetry = do
  _ <- A.string "T#"
  s <- parseSeq
  vals <- replicateM 5 (A.double <* ",")
  bits <- replicateM 8 (A.satisfy (`elem` ['0', '1']))
  let [(bits', "")] = readInt 2 (`elem` ['0', '1']) (\c -> if c == '0' then 0 else 1) bits
  rest <- A.takeText

  return $ TelemetryPacket s vals (fromInteger bits') rest

  where
    parseSeq :: A.Parser Text
    parseSeq = ("MIC" *> (A.string "," <|> pure "") >> pure "MIC") <|> do
      s <- replicateM 3 A.anyChar
      _ <- A.string ","
      return (fromString s)

