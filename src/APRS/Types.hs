{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module APRS.Types
    ( PacketType(..)
    , Address
    , address
    , Similar
    , (≈)
    , Frame(..)
    , Position(..)
    , Timestamp(..)
    , WeatherParam(..)
    , identifyPacket
    , callPass
    , decodeBase91
    -- New parser
    , APRSPacket(..)
    , PosExtension(..)
    , Symbol(..)
    , Directivity(..)
    , MessageInfo(..)
    , ObjectState(..)
    , position
    -- parsers
    , parseAddr
    , parseFrame
    , parseTimestamp
    , parseWeather
    , bodyParser
    -- For testing
    ) where

import Data.Char (chr)
import Control.Applicative ((<|>))
import Control.Monad (replicateM, replicateM_, guard)
import Data.Bits (xor, (.&.), shiftL)
import Data.Char (digitToInt)
import Data.Either (either)
import Data.Maybe (catMaybes)
import Data.Int (Int16)
import Data.Word (Word8)
import Data.String (fromString)
import Data.Text (Text, any, all, length, unpack)
import Text.Read (readMaybe)
import Numeric (readInt)
import Prelude hiding (any)
import qualified Data.Attoparsec.Text as A

import qualified APRS.MicE as M

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
  | RawGPSOrUlt
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
identifyPacket '$' = RawGPSOrUlt
identifyPacket '>' = Status
identifyPacket '?' = Query
identifyPacket '@' = PositionMsg
identifyPacket 'T' = Telemetry
identifyPacket '_' = WeatherNoPos
identifyPacket '`' = CurrentMicE
identifyPacket '{' = UserDefined
identifyPacket '}' = ThirdParty
identifyPacket x = InvalidPacket x

validPktTypes :: [Char]
validPktTypes = map (chr.fromIntegral) $ filter isValid [minBound..maxBound]
  where
    isValid :: Word8 -> Bool
    isValid w = case (identifyPacket.chr.fromIntegral) w of
                  InvalidPacket _ -> False
                  _ -> True

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
  ss <- ("-" *> A.takeWhile (A.inClass "A-Z0-9")) <|> pure ""
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

position :: Frame -> Maybe Position
position (Frame _ _ _ (PositionPacket _ _ pos _ _)) = Just pos
position (Frame _ _ _ (ObjectPacket _ _ _ pos _ _)) = Just pos
position (Frame _ _ _ (ItemPacket _ _ _ pos _))     = Just pos
position (Frame _ _ _ (WeatherPacket _ mpos _ _))   = mpos
position (Frame _ _ _ (MicEPacket _ _ pos _))       = Just pos
position _                                          = Nothing

data Timestamp = DHMLocal (Int, Int, Int)
               | DHMZulu (Int, Int, Int)
               | HMS (Int, Int, Int)
               | MDHM (Int, Int, Int, Int)
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

-- data Position = Position { _pos :: Geodetic WGS84, _ambiguity :: Int }
-- lon, lat, velocity
-- TODO: Perhaps stuff some extensions in here to cover PosExtension
-- as well as compression type and ambiguity.
newtype Position = Position (Double, Double, PosExtension) deriving (Eq, Show)

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
data Frame = Frame Address Address [Text] APRSPacket
           deriving (Show, Eq)

parseFrame :: A.Parser Frame
parseFrame = do
  src <- parseAddr
  _ <- A.char '>'
  dest <- parseAddr
  _ <- A.string "," <|> pure "" -- maybe comma
  path <- A.sepBy (A.takeWhile (`notElem` [',', ':'])) (A.char ',')
  _ <- A.char ':'
  bod <- bodyParser dest
  return $ Frame src dest path bod

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

data PosExtension = PosECourseSpeed Int Double
                  | PosEPHG Int Int Int Directivity
                  | PosERNG -- TODO
                  | PosEDFS -- TODO
                  | PosETypeDesc -- TODO
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
      return $ PosECourseSpeed (if c == 360 then 0 else c) (fromIntegral s * 1.852)

    parsePHG = do
      _ <- A.string "PHG"
      p <- A.digit
      h <- A.digit
      g <- A.digit
      d <- A.satisfy (A.inClass "0-8")

      let d' = (toEnum . digitToInt) d :: Directivity

      return $ PosEPHG (digitToInt p ^ 2) (10 * 2 ^ digitToInt h) (digitToInt g) d'

data Symbol = Symbol Char Char deriving (Show, Eq)

data MessageInfo = Message Text
                 | MessageACK
                 | MessageNAK
                   deriving (Show, Eq)

-- TODO:  Include extensions from page 27 in position packets
data APRSPacket = PositionPacket PacketType Symbol Position (Maybe Timestamp) Text
                | ObjectPacket Symbol ObjectState Text Position Timestamp Text
                | ItemPacket Symbol ObjectState Text Position Text
                | WeatherPacket (Maybe Timestamp) (Maybe Position) [WeatherParam] Text
                | StatusPacket (Maybe Timestamp) Text
                | MessagePacket Address MessageInfo Text -- includes sequence number
                | TelemetryPacket Text [Double] Word8 Text -- seq, vals, bits, comment
                | MicEPacket Symbol Int Position Text
                | NotImplemented PacketType Text
                | GarbagePacket Text
                deriving (Show, Eq)

bodyParser :: Address -> A.Parser APRSPacket
bodyParser dest = parseWeatherPacket
                  <|> parseObjectPacket
                  <|> parseItemPacket
                  <|> parseStatusPacket
                  <|> parseMessagePacket
                  <|> parseTelemetry
                  <|> parseMicE dest
                  <|> parsePositionPacket
                  <|> parseNotImplemented
                  <|> (A.takeText >>= pure.GarbagePacket)


parseNotImplemented :: A.Parser APRSPacket
parseNotImplemented = do
  c <- A.satisfy (`elem` validPktTypes)
  t <- A.takeText
  return $ NotImplemented (identifyPacket c) t

{-
|       | No MSG | MSG |
| NO TS | !      | =   |
| TS    | /      | @   |
-}

parsePosition :: A.Parser (Symbol, Position)
parsePosition = parsePosUncompressed <|> parsePosCompressed

parsePosCompressed :: A.Parser (Symbol, Position)
parsePosCompressed = do
  tbl <- A.anyChar
  b91a <- parseB91Seg
  b91b <- parseB91Seg
  sym <- A.anyChar -- symbol code
  vel <- replicateM 2 A.anyChar
  _ <- A.anyChar -- TODO:  compression type

  return (Symbol tbl sym, Position $ unc b91a b91b $ pcvel $ map fromEnum vel)

  where
    unc m1 m2 v = (90 - (m1 / 380926), (-180) + (m2 / 190463), v)
    pcvel :: [Int] -> PosExtension
    pcvel [a,b]
      | a >= 33 && a <= 122 = let course = fromIntegral (a - 33) * 4
                                  speed = 1.852 * ((1.08 ^ (b - 33)) - 1)
                                  course' = if course == 0 then 360 else course in
                                PosECourseSpeed course' speed
    pcvel _ = PosENone


parsePosUncompressed :: A.Parser (Symbol, Position)
parsePosUncompressed = do
  lat <- parseDir 2
  tbl <- A.satisfy (A.inClass "0-9/\\A-Za-j")
  lon <- parseDir 3
  sym <- A.anyChar
  posE <- parsePosExtension

  return (Symbol tbl sym, Position (lat,lon, posE))

  where
    parseDir :: Int -> A.Parser Double
    parseDir n = do
      cM <- replicateM n A.digit
      cm <- A.many' A.digit
      cs1 <- A.many' A.space
      _ <- A.char '.'
      cd <- A.many' A.digit
      cs2 <- A.many' A.space
      cdir <- A.satisfy (`elem` ['N', 'S', 'E', 'W'])

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


{-
Note: There is one exception to the requirement for the Data Type Identifier
to be the first character in the Information field — this is the Position without
Timestamp (indicated by the ! DTI). The ! character may occur anywhere
up to and including the 40th character position in the Information field. This
variability is required to support X1J TNC digipeaters which have a string of
unmodifiable text at the beginning of the field.
-}

parsePositionPacket :: A.Parser APRSPacket
parsePositionPacket = do
  pre <- A.satisfy (`elem` ['!', '=', '/', '@']) <|> bangjunk
  ts <- maybeTS pre
  (sym, pos) <- parsePosition
  com <- A.takeText
  return $ PositionPacket (identifyPacket pre) sym pos ts com

  where
    maybeTS :: Char -> A.Parser (Maybe Timestamp)
    maybeTS '!' = pure Nothing
    maybeTS '=' = pure Nothing
    maybeTS _ = Just <$> parseTimestamp

    bangjunk :: A.Parser Char
    bangjunk = do
      pre <- A.takeTill (== '!')
      guard $ Data.Text.length pre <= 40
      A.char '!'

data ObjectState = Live | Killed deriving (Show, Eq)

objState :: Char -> ObjectState
objState '_' = Killed
objState _ = Live

parseObjectPacket :: A.Parser APRSPacket
parseObjectPacket = do
  _ <- A.satisfy (== ';')
  name <- replicateM 9 A.anyChar
  ost <- A.satisfy (`elem` ['_', '*']) -- killed, live
  ts <- parseTimestamp
  (sym, Position (lat,lon,_)) <- parsePosition
  comment <- A.takeText
  return $ ObjectPacket sym (objState ost) (fromString name) (Position (lat, lon, PosENone)) ts comment

parseItemPacket :: A.Parser APRSPacket
parseItemPacket = do
  _ <- A.satisfy (== ')')
  name <- A.takeTill (\c -> c == '_' || c == '!')
  guard $ Data.Text.length name >= 3 && Data.Text.length name <= 9
  ost <- A.satisfy (`elem` ['_', '!']) -- killed, live
  (sym, Position (lat,lon,_)) <- parsePosition
  comment <- A.takeText
  return $ ItemPacket sym (objState ost) name (Position (lat, lon, PosENone)) comment

-- Examples that aren't recognized properly:
--   @092345z/5L!!<*e7 _7P[g005t077r000p000P000h50b09900wRSW
parseWeatherPacket :: A.Parser APRSPacket
parseWeatherPacket = parseUltimeter <|> parseStandardWeather

{-
1. Wind Speed (0.1 kph)
2. Wind Direction (0-255)
3. Outdoor Temp (0.1 deg F)
4. Rain* Long Term Total (0.01 inches)
5. Barometer (0.1 mbar)
6. Indoor Temp (0.1 deg F)
7. Outdoor Humidity (0.1%)
8. Indoor Humidity (0.1%)
9. Date (day of year)     -- TODO
10. Time (minute of day)  -- TODO
11. Today's Rain Total (0.01 inches)*
12. 1 Minute Wind Speed Average (0.1kph)*
-}


-- This is data logger mode.  There's also a || for packet mode.  See
-- http://www.peetbros.com/shop/custom.aspx?recid=29
-- TODO:  Ensure consistent units
parseUltimeter :: A.Parser APRSPacket
parseUltimeter = do
  _ <- A.string "!!"
  vals <- replicateM 12 aValue
  let funs = [(Just.)WindSpeed, (Just.)WindDir, (Just.)Temp, (Just.)RainLast24Hours,
              (Just.)Baro, ignore, (Just.)Humidity, ignore, ignore, ignore,
              (Just.)RainToday, ignore]
  let params = zipWith (=<<) funs vals
  return $ WeatherPacket Nothing Nothing (catMaybes params) ""

  where aValue :: A.Parser (Maybe Int)
        aValue = do
          digs <- replicateM 4 (A.satisfy $ A.inClass "A-F0-9-")
          return $ readMaybe digs
        ignore = const Nothing

parseStandardWeather :: A.Parser APRSPacket
parseStandardWeather = do
  c <- A.satisfy (`elem` ['_', '/', '!', '@', '='])
  ts <- (parseTimestamp >>= pure.Just) <|> pure Nothing
  pos <- if c `elem` ['_', '='] then pure Nothing else ppos
  let extra = case pos of
                (Just (Position (_,_,PosECourseSpeed a b))) -> [WindDir a, WindSpeed (round $ b / 1.852)]
                _ -> []
  wp <- parseWeather
  rest <- A.takeText
  return $ WeatherPacket ts (pos' pos) (extra ++ wp) rest

  where
    ppos :: A.Parser (Maybe Position)
    ppos = parsePosition >>= \(_, p) -> return $ Just p
    pos' Nothing = Nothing
    pos' (Just (Position (a,b,_))) = Just (Position (a,b,PosENone))

parseStatusPacket :: A.Parser APRSPacket
parseStatusPacket = do
  _ <- A.char '>'
  ts <- (parseTimestamp >>= pure.Just) <|> pure Nothing
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
      return (Message (fromString mtext), mid)

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
    parseSeq = ("MIC" *> (A.string "," <|> pure "") >> pure "MIC")
               <|> (replicateM 3 A.anyChar <* "," >>= pure.fromString)
               <|> (A.takeWhile (A.inClass "0-9")) <* ","

parseMicE :: Address -> A.Parser APRSPacket
parseMicE (Address call ss) = do
  _ <- A.satisfy (`elem` ['\'', '`'])
  let (lat, mbits, off, sign, _path) = M.micEDest call ss
  [d,m,h] <- replicateM 3 A.anyChar

  let lon' = (fromIntegral.M.micELonD d) off +
        (((fromIntegral.M.micELonM) m + ((fromIntegral.fromEnum) h - 28) / 100) / 60)
  let lon = (fromIntegral sign) * lon'

  [sp,dc,se] <- replicateM 3 A.anyChar
  let speed10 = (fromEnum sp - 28) * 10
  let (speed1, course100) = (fromEnum dc - 28) `quotRem` 10
  let course' = (fromEnum se - 28)
  let speed = (speed10 + speed1) `mod` 800
  let course = course' + (course100*100)
  let ext = PosECourseSpeed (if course > 400 then course - 400 else course) (fromIntegral speed)
  sym <- A.anyChar
  tbl <- A.anyChar

  return $ MicEPacket (Symbol tbl sym) mbits (Position (lat,lon,ext)) ""
