{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module APRS.Types
    ( PacketType(..)
    , Address
    , address
    , unAddress
    , Similar ((≈)), elemish
    , Frame(..)
    , Position(..)
    , Timestamp(..)
    , WeatherParam(..)
    , WeatherSW(..)
    , WeatherUnit(..)
    , identifyPacket
    , callPass
    , decodeBase91
    , symbol
    , distance
    -- New parser
    , APRSPacket(..)
    , PosExtension(..)
    , Symbol(..)
    , Directivity(..)
    , MessageInfo(..)
    , ObjectState(..)
    , ObjectData(..)
    , Capability(..)
    , position
    -- parsers
    , parseAddr
    , parseFrame
    , parseTimestamp
    , parseWeather
    , bodyParser
    -- For testing
    , lookupWeatherSW
    ) where

import           Control.Applicative               ((<|>))
import           Control.Monad                     (guard, replicateM, replicateM_)
import qualified Data.Attoparsec.Text              as A
import           Data.Bits                         (shiftL, xor, (.&.))
import           Data.Char                         (digitToInt, toLower)
import           Data.Foldable                     (any, foldl')
import           Data.Functor                      (($>))
import           Data.Int                          (Int16)
import           Data.Maybe                        (catMaybes, fromMaybe)
import           Data.String                       (fromString)
import           Data.Text                         (Text, all, any, length, unpack)
import           Data.Word                         (Word8)
import           Geodetics.Geodetic                (Geodetic (..), WGS84 (..), groundDistance)
import           Numeric                           (readInt)
import           Numeric.Units.Dimensional         ((*~), (/~), _0)
import           Numeric.Units.Dimensional.SIUnits (degree, meter)
import           Text.Read                         (readMaybe)

import qualified APRS.MicE                         as M
import qualified APRS.NMEA                         as N

class Similar a where
  (≈) :: a -> a -> Bool

elemish :: (Foldable f, Similar a) => a -> f a -> Bool
elemish n = Data.Foldable.any (≈ n)

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
        invalid = Data.Text.any (`notElem` addrChars)

unAddress :: Address -> (Text, Text)
unAddress (Address c s) = (c, s)

instance Show Address where
  show (Address c "") = unpack c
  show (Address c s) = unpack c ++ "-" ++ unpack s

parseAddr :: A.Parser Address
parseAddr = do
  c <- A.takeWhile (A.inClass "A-Z0-9")
  ss <- ("-" *> A.takeWhile (A.inClass "A-Z0-9")) <|> pure ""
  either fail pure $ address c ss

instance Read Address where
  readsPrec _ x = either error (\a -> [(a,"")]) $ A.parseOnly parseAddr (fromString x)

ctoi :: Char -> Int16
ctoi = toEnum . fromEnum

callPass :: Address -> Int16
callPass (Address a _) =
  0x7fff .&. foldl' xor 0x73e2 (map (\(c, f) -> f (ctoi c)) $ zip a' $ cycle [flip shiftL 8, id])
  where a' = unpack a

instance Similar Address where
  (≈) (Address a _) (Address b _) = a == b

b91chars :: String
b91chars = "[!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\^_`abcdefghijklmnopqrstuvwxyz{]"

parseB91Seg :: Int -> A.Parser Double
parseB91Seg n = fromIntegral.decodeBase91.fromString <$> replicateM n (A.satisfy (`elem` b91chars))

posamb :: Int -> Double
posamb 0 = 0
posamb 1 = 0.05 / 60
posamb 2 = 0.5 / 60
posamb 3 = 5 / 60
posamb 4 = 0.5
posamb _ = error "Invalid ambiguity"

position :: Frame -> Maybe Position
position (Frame _ _ _ (PositionPacket _ _ pos _ _))   = Just pos
position (Frame _ _ _ (ObjectPacket _ _ _ pos _ _))   = Just pos
position (Frame _ _ _ (ItemPacket _ _ _ pos _))       = Just pos
position (Frame _ _ _ (WeatherPacket _ mpos _ _ _ _)) = mpos
position (Frame _ _ _ (MicEPacket _ _ pos _))         = Just pos
position _                                            = Nothing

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
    n x = replicateM x (replicateM 2 A.digit) >>= \digs -> pure $ map read digs
    n3 :: Char -> A.Parser (Int, Int, Int)
    n3 ch = n 3 >>= \[a,b,c] -> A.char ch >> pure (a,b,c)

-- data Position = Position { _pos :: Geodetic WGS84, _ambiguity :: Int }
-- lon, lat, alt, extension
-- TODO: Perhaps stuff some extensions in here to cover PosExtension
-- as well as compression type and ambiguity.
newtype Position = Position (Double, Double, Double, PosExtension) deriving (Eq, Show)

distance :: Position -> Position -> Double
distance a b = case groundDistance (g a) (g b) of
                 Just (d,_,_) -> let d' = d /~ meter in if isNaN d' then 0 else d'
                 _            -> 0

  where g (Position (lon, lat, _, _)) = Geodetic (lat *~ degree) (lon *~ degree) _0 WGS84

data WeatherParam = WindDir Int
                  | WindSpeed Int
                  | WindGust Int
                  | Temp Double
                  | RainLastHour Int
                  | RainLast24Hours Int
                  | RainToday Int
                  | Humidity Int
                  | Baro Int
                  | Voltage Double
                  | WaterLevel Double
                  | Luminosity Int
                  | Snowfall Int
                  | RawRain Int
                  | NoData Char
                  deriving (Show, Eq)

parseWParam :: A.Parser WeatherParam
parseWParam = w 'c' WindDir
              <|> w 's' WindSpeed
              <|> w 'g' WindGust
              <|> w 't' (Temp . f2c . fromIntegral)
              <|> w 'r' RainLastHour
              <|> w 'p' RainLast24Hours
              <|> w 'P' RainToday
              <|> w 'F' (WaterLevel . (/ 10) . fromIntegral)
              <|> w 'V' (Voltage . (/ 10) . fromIntegral)
              <|> w' 2 'h' Humidity
              <|> w' 5 'b' Baro
              <|> w 'L' Luminosity
              <|> w 'l' (Luminosity . (+ 1000))
              <|> w '#' RawRain
  where
    w' :: Int -> Char -> (Int -> WeatherParam) -> A.Parser WeatherParam
    w' i c wc = do
      _ <- A.char c
      wgood i wc <|> wnodata i c

    wgood i wc = replicateM i A.digit >>= \deez -> pure $ wc (read deez)
    wnodata i c = replicateM_ i (A.satisfy (`elem` ['.', ' '])) >> pure (NoData c)

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
  pure $ Frame src dest path bod

decodeBase91 :: String -> Int
decodeBase91 s =
  let l = Prelude.length s - 1 in
    foldl' (\a (c, i) -> i * ((toEnum . fromEnum $ c) -33) + a) 0 $ zip s [91^x | x <- [l,pred l..0]]

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

-- Parse common aaa/bbb to (a,b)
parseCourseSpeed :: A.Parser (Int, Double)
parseCourseSpeed = do
  a <- replicateM 3 A.digit
  _ <- A.char '/'
  b <- replicateM 3 A.digit
  pure (read a `mod` 360, fromIntegral (read b) * 1.852)

data Directivity = Omni | DirNE | DirE | DirSE | DirS | DirSW | DirW | DirNW | DirN
                 deriving (Show, Eq, Ord, Enum, Bounded)

parsePosExtension :: A.Parser PosExtension
parsePosExtension = (parseCourseSpeed >>= uncurry ((pure .) . PosECourseSpeed))
                    <|> parsePHG
                    <|> pure PosENone

  where
    parsePHG = do
      _ <- A.string "PHG"
      p <- A.digit
      h <- A.digit
      g <- A.digit
      d <- A.satisfy (A.inClass "0-8")

      let d' = (toEnum . digitToInt) d :: Directivity

      pure $ PosEPHG (digitToInt p ^ 2) (10 * 2 ^ digitToInt h) (digitToInt g) d'

data Symbol = Symbol Char Char deriving (Show, Eq)

data MessageInfo = Message Text
                 | MessageACK
                 | MessageNAK
                   deriving (Show, Eq)

data Capability = IGATE
                | MessageCount Int
                | LocalCount Int
                | Capability Text Text
                deriving (Show, Eq)

data WeatherSW = APRSdos
               | MacAPRS
               | PocketAPRS
               | APRSSA
               | WinAPRS
               | XAPRS
               | OpenTracker
               | Kenwood
               | Byonics
               | Yaesu
               | UnknownWeatherSW Char
               deriving (Eq, Ord, Show)

weatherSWMap :: [(Char, WeatherSW)]
weatherSWMap = [('d', APRSdos),
                ('m', MacAPRS),
                ('p', PocketAPRS),
                ('s', APRSSA),
                ('w', WinAPRS),
                ('x', XAPRS),
                ('o', OpenTracker),
                ('k', Kenwood),
                ('b', Byonics),
                ('y', Yaesu)]

lookupWeatherSW :: Char -> WeatherSW
lookupWeatherSW c = fromMaybe (UnknownWeatherSW c) (lookup (toLower c) weatherSWMap)

data WeatherUnit = WUDavis
                 | WUDavisVantagePro
                 | WUHeathkit
                 | WUPIC
                 | WURadioShack
                 | WUUltimeterIIAuto
                 | WUUltimeterIIRemote
                 | WUUltimeter2000
                 | WUUltimeterRemote
                 | WUUltimeter500
                 | WURemoteUltimeterPacket
                 | WUOpenTrackerTW1
                 | UnknownWeatherUnit Text
                 deriving (Show, Eq)

parseWeatherUnit :: A.Parser WeatherUnit
parseWeatherUnit =
  "Dvs"           $> WUDavis
  <|> "DsVP"      $> WUDavisVantagePro
  <|> "HKT"       $> WUHeathkit
  <|> "PIC"       $> WUPIC
  <|> "RSW"       $> WURadioShack
  <|> "U-II"      $> WUUltimeterIIAuto
  <|> "U2R"       $> WUUltimeterIIRemote
  <|> "U2k"       $> WUUltimeter2000
  <|> "U2kr"      $> WUUltimeterRemote
  <|> "U5"        $> WUUltimeter500
  <|> "Upkm"      $> WURemoteUltimeterPacket
  <|> "TW1"       $> WUOpenTrackerTW1
  <|> (A.take 2  >>= (pure.) UnknownWeatherUnit)

data ObjectData = ObjText Text
                | ObjWeather [WeatherParam]
                deriving (Show, Eq)

-- TODO:  Include extensions from page 27 in position packets
data APRSPacket = PositionPacket PacketType Symbol Position (Maybe Timestamp) Text
                | ObjectPacket Symbol ObjectState Text Position Timestamp ObjectData
                | ItemPacket Symbol ObjectState Text Position Text
                | WeatherPacket (Maybe Timestamp) (Maybe Position) [WeatherParam] WeatherSW WeatherUnit Text
                | StatusPacket (Maybe Timestamp) Text
                | MessagePacket Address MessageInfo Text -- includes sequence number
                | TelemetryPacket Text [Double] Word8 Text -- seq, vals, bits, comment
                | MicEPacket Symbol Int Position Text
                | RawGPSPacket Position Timestamp
                | CapabilitiesPacket [Capability]
                | NotUnderstoodPacket Text
                deriving (Show, Eq)

symbol :: Frame -> Maybe Symbol
symbol (Frame _ _ _ (PositionPacket _ s _ _ _)) = Just s
symbol (Frame _ _ _ (ObjectPacket s _ _ _ _ _)) = Just s
symbol (Frame _ _ _ (ItemPacket s _ _ _ _)) = Just s
symbol (Frame _ _ _ (MicEPacket s _ _ _)) = Just s
symbol _ = Nothing


bodyParser :: Address -> A.Parser APRSPacket
bodyParser dest = parseWeatherPacket
                  <|> parseObjectPacket
                  <|> parseItemPacket
                  <|> parseStatusPacket
                  <|> parseCapabilityPacket
                  <|> parseMessagePacket
                  <|> parseTelemetry
                  <|> parseMicE dest
                  <|> parseNMEA
                  <|> parsePositionPacket
                  <|> (NotUnderstoodPacket <$> A.takeText)

parseNMEA :: A.Parser APRSPacket
parseNMEA = do
  (lat,lon,ts) <- N.parseNMEA
  -- TODO: Altitude
  pure $ RawGPSPacket (Position (lat,lon,0,PosENone)) (HMS ts)

parseCapabilityPacket :: A.Parser APRSPacket
parseCapabilityPacket = do
  _ <- A.char '<'
  caps <- A.sepBy cap (A.satisfy isSep)
  pure $ CapabilitiesPacket caps

  where
    isSep = (`elem` [' ', ','])
    cap :: A.Parser Capability
    cap = (A.string "IGATE" >> pure IGATE)
          <|> "MSG_CNT=" *> (MessageCount <$> A.decimal)
          <|> "LOC_CNT=" *> (LocalCount <$> A.decimal)
          <|> other

    other :: A.Parser Capability
    other = do
      k <- A.takeTill (== '=')
      _ <- A.satisfy (`elem` [' ', '='])
      v <- A.takeWhile (not.isSep)
      pure $ Capability k v

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
  b91a <- parseB91Seg 4
  b91b <- parseB91Seg 4
  sym <- A.anyChar -- symbol code

  -- It seems that xastir will just truncate the packet once it has enough stuff.
  ext <- pcvel <|> pure PosENone

  pure (Symbol tbl sym, Position $ unc b91a b91b ext)

  where
    -- TODO: Altitude
    unc m1 m2 v = (90 - (m1 / 380926), (-180) + (m2 / 190463), 0, v)
    pcvel :: A.Parser PosExtension
    pcvel = pcvel' . map fromEnum <$> replicateM 3 A.anyChar

    pcvel' [a,b,_]
      | a >= 33 && a <= 122 = let course = fromIntegral (a - 33) * 4
                                  speed = 1.852 * ((1.08 ^ (b - 33)) - 1)
                                  course' = if course == 0 then 360 else course in
                                PosECourseSpeed course' speed
    pcvel' _ = PosENone


parsePosUncompressed :: A.Parser (Symbol, Position)
parsePosUncompressed = do
  lat <- parseDir 2
  tbl <- A.satisfy (A.inClass "0-9/\\A-Za-j")
  lon <- parseDir 3
  sym <- A.anyChar
  posE <- parsePosExtension

  -- TODO:  Altitude
  pure (Symbol tbl sym, Position (lat,lon,0,posE))

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
      pure $ (compPos cM (cm ++ cs1) (cd ++ cs2) + posamb amb) * psign cdir

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
  PositionPacket (identifyPacket pre) sym pos ts <$> A.takeText

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

data ObjectState = Live | Killed deriving (Show, Enum, Bounded, Eq)

objState :: Char -> ObjectState
objState '_' = Killed
objState _ = Live

parseObjectPacket :: A.Parser APRSPacket
parseObjectPacket = do
  _ <- A.satisfy (== ';')
  name <- replicateM 9 A.anyChar
  ost <- A.satisfy (`elem` ['_', '*']) -- killed, live
  ts <- parseTimestamp
  (sym, Position (lat,lon,alt,_)) <- parsePosition
  objdat <- parseObjData
  pure $ ObjectPacket sym (objState ost) (fromString name) (Position (lat, lon, alt, PosENone)) ts objdat

  where
    parseObjData :: A.Parser ObjectData
    parseObjData = (ObjWeather <$> parseWeather) <|> (ObjText <$> A.takeText)

parseItemPacket :: A.Parser APRSPacket
parseItemPacket = do
  _ <- A.satisfy (== ')')
  name <- A.takeTill (\c -> c == '_' || c == '!')
  guard $ Data.Text.length name >= 3 && Data.Text.length name <= 9
  ost <- A.satisfy (`elem` ['_', '!']) -- killed, live
  (sym, Position (lat,lon,alt,_)) <- parsePosition
  ItemPacket sym (objState ost) name (Position (lat, lon, alt, PosENone)) <$> A.takeText

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
  let funsl = [(Just.)WindSpeed.(`div` 10), (Just.)WindDir, (Just.)convertTemp, (Just.)RainLast24Hours,
               (Just.)Baro.(`div` 10), ignore, (Just.)Humidity.(`div` 10), ignore, ignore, ignore,
               (Just.)RainToday, ignore]
  let funsp = [(Just.)WindSpeed.(`div` 10), (Just.)WindDir, (Just.)convertTemp, (Just.)RainLast24Hours,
               (Just.)Baro.(`div` 10), ignore, ignore, ignore, (Just.)Humidity.(`div` 10), ignore, ignore,
               (Just.)RainToday]

  "!!" *> ulti funsl <|> "$ULTW" *> ulti funsp

  where
    ulti :: [Int -> Maybe WeatherParam] -> A.Parser APRSPacket
    ulti funs = do
      v <- vals
      let params = zipWith (=<<) funs v
      pure $ WeatherPacket Nothing Nothing (catMaybes params) (UnknownWeatherSW '?') WUUltimeter2000 ""

    vals = A.many1 $ do
      digs <- replicateM 4 (A.satisfy $ A.inClass "A-F0-9-")
      pure $ readMaybe ("0x" ++ digs)

    ignore = const Nothing

    convertTemp :: Int -> WeatherParam
    convertTemp x = Temp $ f2c (fromIntegral x / 10)

f2c :: Double -> Double
f2c x = (x - 32) * 5 / 9

parseStandardWeather :: A.Parser APRSPacket
parseStandardWeather = do
  c <- A.satisfy (`elem` ['_', '/', '!', '@', '='])
  ts <- (Just <$> parseTimestamp) <|> pure Nothing
  pos <- if c `elem` ['_', '='] then pure Nothing else ppos
  let extra = case pos of
                (Just (Position (_,_,_,PosECourseSpeed a b))) -> [WindDir a, WindSpeed (round $ b / 1.852)]
                _                                             -> []
  ws <- parsews <|> pure []
  wp <- parseWeather
  swc <- A.anyChar <|> pure '?'
  unit <- parseWeatherUnit <|> pure (UnknownWeatherUnit "??")
  WeatherPacket ts (pos' pos) (mightsnow $ extra ++ ws ++ wp) (lookupWeatherSW swc) unit <$> A.takeText

  where
    ppos :: A.Parser (Maybe Position)
    ppos = parsePosition >>= \(_, p) -> pure $ Just p
    pos' Nothing = Nothing
    pos' (Just (Position (a,b,alt,_))) = Just (Position (a,b,alt,PosENone))
    parsews = do
      (crs, spd) <- parseCourseSpeed
      pure [WindDir crs, WindSpeed (round spd)]
    mightsnow :: [WeatherParam] -> [WeatherParam]
    mightsnow [] = []
    mightsnow (s@(WindSpeed _):xs) = s : sawRain xs
    mightsnow (x:xs) = x : mightsnow xs

    sawRain [] = []
    sawRain (WindSpeed s:xs) = Snowfall s : sawRain xs
    sawRain (x:xs) = x : sawRain xs

parseStatusPacket :: A.Parser APRSPacket
parseStatusPacket = do
  _ <- A.char '>'
  ts <- (Just <$> parseTimestamp) <|> pure Nothing
  msg <- A.many1 (A.satisfy (`notElem` ['|', '~']))
  pure $ StatusPacket ts (fromString msg)

parseMessagePacket :: A.Parser APRSPacket
parseMessagePacket = do
  _ <- A.char ':' -- message indicator
  rcpt <- parseAddr
  _ <- A.many' A.space -- Technically, rpct is 9 characters with trailing space, but we're Posteling here a bit.
  _ <- A.char ':' -- message separator
  (mi, mid) <- parseMI

  pure $ MessagePacket rcpt mi mid

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
      pure (i, msgid)

    parseMsg :: A.Parser (MessageInfo, Text)
    parseMsg = do
      mtext <- A.many' (A.satisfy (`notElem` ['{', '|', '~']))
      mid <- ("{" *> A.takeText) <|> pure "" -- message ID is optional
      pure (Message (fromString mtext), mid)

parseTelemetry :: A.Parser APRSPacket
parseTelemetry = do
  _ <- A.string "T#"
  s <- parseSeq
  vals <- replicateM 5 (A.double <* ",")
  bits <- replicateM 8 (A.satisfy (`elem` ['0', '1']))
  let [(bits', "")] = readInt 2 (`elem` ['0', '1']) (\c -> if c == '0' then 0 else 1) bits
  TelemetryPacket s vals (fromInteger bits') <$> A.takeText

  where
    parseSeq :: A.Parser Text
    parseSeq = ("MIC" *> (A.string "," <|> pure "") >> pure "MIC")
               <|> fromString <$> (replicateM 3 A.anyChar <* ",")
               <|> A.takeWhile (A.inClass "0-9") <* ","

parseMicE :: Address -> A.Parser APRSPacket
parseMicE (Address call ss) = do
  _ <- A.satisfy (`elem` ['\'', '`'])
  let (lat, mbits, off, sign, _path) = M.micEDest call ss
  [d,m,h] <- replicateM 3 A.anyChar

  let lon' = (fromIntegral.M.micELonD d) off +
        (((fromIntegral.M.micELonM) m + ((fromIntegral.fromEnum) h - 28) / 100) / 60)
  let lon = fromIntegral sign * lon'

  [sp,dc,se] <- replicateM 3 A.anyChar
  let speed10 = (fromEnum sp - 28) * 10
  let (speed1, course100) = (fromEnum dc - 28) `quotRem` 10
  let course' = fromEnum se - 28
  let speed = (speed10 + speed1) `mod` 800
  let course = course' + (course100*100)
  let ext = PosECourseSpeed (if course > 400 then course - 400 else course) (fromIntegral speed)
  sym <- A.anyChar
  tbl <- A.anyChar
  alt <- micalt
  MicEPacket (Symbol tbl sym) mbits (Position (lat,lon,alt-10000,ext)) <$> A.takeText

  where micalt = (A.satisfy (`elem` ['\'', '`', ' ', ']', '>', 'T']) >> decodeAlt) <|> decodeAlt
        decodeAlt = (parseB91Seg 3 <* "}") <|> pure 10000 -- 10km = sea level.
