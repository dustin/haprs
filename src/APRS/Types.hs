{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module APRS.Types
    ( PacketType
    , must
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
    -- For testing
    , splitWith
    , splitOn'
    ) where

import Prelude hiding (any, take, drop, head, takeWhile)
import Control.Applicative ((<|>), liftA2)
import Data.Char (isDigit)
import Data.String (fromString)
import Data.Text (Text, any, take, drop, head, uncons, dropAround, splitOn,
                  takeWhile, length, index, intercalate, unpack, concat)
import Data.Bits (xor, (.&.), shiftL)
import Data.Int (Int16)
import Text.Regex (Regex, mkRegex, matchRegex)
import Text.Read (readMaybe)

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

data Address = Address { _call :: !Text, _ssid :: !Text } deriving (Eq)

addrChars :: [Char]
addrChars = ['A'..'Z'] ++ ['0'..'9']

must :: Either String a -> a
must (Left x) = error x
must (Right x) = x

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

splitOn' :: (Eq a) => a -> [a] -> ([a], [a])
splitOn' c = splitWith (== c)

splitWith :: (a -> Bool) -> [a] -> ([a], [a])
splitWith f s =
  let go l [] = (reverse l, [])
      go l (x:r)
        | f x = (reverse l, r)
        | otherwise = go (x:l) r in go [] s

instance Read Address where
  readsPrec _ x = [let (l, r) = splitOn' '-' x
                       (u, xtra) = splitWith (not . (`elem` addrChars)) r in
                     (must $ address (fromString l) (fromString u), xtra)]

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

coordField :: String
coordField = "(\\d{1,3})([0-5 ][0-9 ])\\.([0-9 ]+)([NEWS])"
b91chars :: String
b91chars = "[!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\^_`abcdefghijklmnopqrstuvwxyz{]"
symbolTables :: String
symbolTables = "[0-9/\\A-z]"

matchText :: Regex -> Text -> Maybe [String]
matchText r t = matchRegex r (unpack t)

uncompressedPositionRe :: Regex
uncompressedPositionRe = mkRegex $ "([!=]|[/@\\*]\\d{6}[hz/])" ++
                         coordField ++ "(" ++ symbolTables ++ ")" ++
                         coordField ++ "(.)([0-3][0-9]{2}/[0-9]{3})?"

matchUncompressed :: Text -> Maybe [String]
matchUncompressed = matchText uncompressedPositionRe
compressedPositionRe :: Regex
compressedPositionRe = mkRegex $ "([!=/@])(" ++
                       b91chars ++ "{4})(" ++ b91chars ++ "{4})(.)(..)(.)"
matchCompressed :: Text -> Maybe [String]
matchCompressed = matchText compressedPositionRe

newtype Velocity = Velocity (Double, Double) deriving (Eq)

instance Show Velocity where
  show (Velocity (course, speed)) = show speed ++ "kph @" ++ show course

-- data Position = Position { _pos :: Geodetic WGS84, _ambiguity :: Int }
-- lon, lat, velocity
newtype Position = Position (Double, Double, Maybe Velocity) deriving (Eq, Show)

position :: Body -> Maybe Position
position bod@(Body bt)
  | Data.Text.length bt < 6 = Nothing
  | otherwise = go $ pktType bod
  where go Nothing = Nothing
        go (Just t)
          | t `elem` [PositionNoTSNoMsg, PositionNoTS]  = newp (drop 1 bt)
          | t `elem` [PositionNoMsg, PositionMsg]       = newp (drop 8 bt)
          | t == Object                                 = newp (drop 19 bt)
          | otherwise                                   = oldp bt
        newp t
          | isDigit (head t) = newPU t
          | otherwise = newPC t
        oldp t = oldPC (matchCompressed t) <|> oldPU (matchUncompressed t)
        oldPC (Just [_m0, m1, m2, _m3, _m4, m5, _m6]) =
          Just $ Position (unc m1 m2 (fmap fromEnum m5))
        oldPC _ = Nothing
        oldPU (Just [_m0, _m1, m2, m3, m4, [m5], _m6, m7, m8, m9, [m10], _m11, m12]) =
          let numstrs = [m2, m3 ++ "." ++ m4, m7, m8 ++ "." ++ m9] in
            parseu numstrs m5 m10 (puvel $ fromString m12)
        oldPU _ = Nothing
        newPU t
          | Data.Text.length t < 19 = Nothing
          | otherwise = let numstrs = [subt 0 2 t, subt 2 5 t, subt 9 3 t, subt 12 5 t] in
                          parseu numstrs (t `index` 7) (t `index` 17) (puvel $ drop 19 t)
        newPC t
          | Data.Text.length t < 12 = Nothing
          | otherwise = Just $ Position $ unc (subt 1 4 t) (subt 5 4 t) ((fmap fromEnum.subt 10 2) t)
        b91f = fromIntegral.decodeBase91 :: String -> Double
        unc m1 m2 m5 = (90 - (b91f m1 / 380926), (-180) + (b91f m2 / 190463), pcvel m5)
        pcvel [a,b]
          | a >= 33 && a <= 122 = let course = fromIntegral (a - 33) * 4
                                      speed = 1.852 * ((1.08 ^ (b - 33)) - 1)
                                      course' = if course == 0 then 360 else course in
                                    Just $ Velocity (course', speed)
        pcvel _ = Nothing
        parseu numstrs d1 d2 v =
          let numstrs' = map (map (\c -> if c == ' ' then '0' else c)) numstrs
              posamb = Prelude.length (filter (== ' ') $ Prelude.concat numstrs) `div` 2 in
            case mapM readMaybe numstrs' of
              Just [a1,a2,b1,b2] -> let a = a1 + (a2 / 60)
                                        b = b1 + (b2 / 60)
                                        offby = case posamb of
                                          0 -> 0
                                          1 -> 0.05 / 60
                                          2 -> 0.5 / 60
                                          3 -> 5 / 60
                                          4 -> 0.5
                                          _ -> error "Invalid ambiguity"
                                        asig = if d1 == 'S' then -1 else 1
                                        bsig = if d2 == 'W' then -1 else 1
                                        a' = (a + offby) * asig
                                        b' = (b + offby) * bsig in
                                      Just $ Position (a', b', v)
              _ -> Nothing
        puvel :: Text -> Maybe Velocity
        puvel x = let as = (unpack.takeWhile isDigit) x
                      bs = (unpack.takeWhile isDigit) $ drop (1 + Prelude.length as) x
                      a = readMaybe as :: Maybe Double
                      b = (* 1.852) <$> readMaybe bs :: Maybe Double
                  in
                    Velocity <$> liftA2 (,) a b

subt' :: Int -> Int -> Text -> Text
subt' s n = take n.drop s

subt :: Int -> Int -> Text -> String
subt s n t = unpack $ subt' s n t

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
                  case rc of
                    Nothing -> Nothing
                    Just rc' -> Just $ Message s rc' bod (Data.Text.concat rest)

data Frame = Frame { source :: Address
                   , dest :: Address
                   , path :: [Text]
                   , body :: Body }
           deriving (Eq)

instance Read Frame where
  readsPrec _ x = [let (addrd, msgd) = splitOn' ':' x
                       (src, dest') = splitOn' '>' addrd
                       (dests, paths) = splitOn' ',' dest' in
                      (Frame { path = map fromString $ words $ map (\c -> if c == ',' then ' ' else c) paths,
                               dest = read dests,
                               source = read src,
                               body = Body (fromString msgd)
                             }, "")]

instance Show Frame where
  show (Frame s d p b) =
    show s ++ ">" ++ show d ++ "," ++ (unpack.intercalate ",") p ++ ":" ++ show b

decodeBase91 :: String -> Int
decodeBase91 s@[_,_,_,_] =
  foldl (\a (c, i) -> i * ((toEnum . fromEnum $ c) -33) + a) 0 $ zip s [91^x | x <- [3,2..0]]
decodeBase91 _ = 0
