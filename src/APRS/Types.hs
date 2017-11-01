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
    , identifyPacket
    , callPass
    , decodeBase91
    -- For testing
    , splitWith
    , splitOn
    ) where

import Data.String (fromString)
import Data.Text (Text, any, intercalate, unpack)
import Data.Bits
import Data.Int

import Geodetics.Geodetic

class Similar a where
  (≈) :: a -> a -> Bool

data PacketType = CurrentMicE
  | Item
  | PositionNoTSNoMsg
  | PositionTSNoMsg
  | Message
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
  | Invalid Char
  deriving (Show, Eq)

identifyPacket :: Char -> PacketType
identifyPacket '\x1c' = CurrentMicE
identifyPacket '!' = PositionNoTSNoMsg
identifyPacket ')' = Item
identifyPacket '/' = PositionNoMsg
identifyPacket ':' = Message
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
identifyPacket x = Invalid x

data Address = Address { _call :: !Text, _ssid :: !Text } deriving (Eq)

addrChars :: [Char]
addrChars = ['A'..'Z'] ++ ['0'..'9']

must :: Either String a -> a
must (Left x) = error x
must (Right x) = x

address :: Text -> Text -> Either String Address
address c s
  | c == "" = Left "callsign is too short"
  | invalid c = Left "invalid characters in callsign"
  | invalid s = Left "invalid characters in SSID"
  | otherwise = Right $ Address c s
  where invalid :: Text -> Bool
        invalid = Data.Text.any (`notElem` addrChars)

instance Show Address where
  show (Address c "") = unpack c
  show (Address c s) = unpack c ++ "-" ++ unpack s

splitOn :: (Eq a) => a -> [a] -> ([a], [a])
splitOn c = splitWith (== c)

splitWith :: (a -> Bool) -> [a] -> ([a], [a])
splitWith f s =
  let go l [] = (reverse l, [])
      go l (x:r)
        | f x = (reverse l, r)
        | otherwise = go (x:l) r in go [] s

instance Read Address where
  readsPrec _ x = [let (l, r) = splitOn '-' x
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

data Position = Position { _pos :: Geodetic WGS84, _ambiguity :: Int }

position :: Body -> Maybe Position
position _ = Nothing

data Frame = Frame { source :: Address
                   , dest :: Address
                   , path :: [Text]
                   , body :: Body }
           deriving (Eq)

instance Read Frame where
  readsPrec _ x = [let (addrd, msgd) = splitOn ':' x
                       (src, dest') = splitOn '>' addrd
                       (dests, paths) = splitOn ',' dest' in
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
