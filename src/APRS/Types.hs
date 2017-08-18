module APRS.Types
    ( PacketType
    , Address
    , address
    , Similar
    , (≈)
    , Frame(..)
    , Body(..)
    , identifyPacket
    , callPass
    , decodeBase91
    ) where

import Data.Bits
import Data.Int
import Data.List

import Geodetics.Geodetic
import Numeric.Units.Dimensional.SIUnits

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

data Address = Address { call :: !String, ssid :: !String } deriving (Eq)

addrChars = ['A'..'Z'] ++ ['0'..'9']

address c s
  | invalid c = error "invalid characters in callsign"
  | invalid s = error "invalid characters in SSID"
  | otherwise = Address c s
  where invalid x = not $ null $ nub x \\ addrChars

instance Show Address where
  show (Address c "") = c
  show (Address c s) = c ++ "-" ++ s

splitOn :: Char -> String -> (String, String)
splitOn c = splitWith (== c)

splitWith :: (Char -> Bool) -> String -> (String, String)
splitWith f s =
  let go l [] = (reverse l, [])
      go l (x:r)
        | f x = (reverse l, r)
        | otherwise = go (x:l) r in go [] s

instance Read Address where
  readsPrec _ x = [let (l, r) = splitOn '-' x
                       (u, xtra) = splitWith (not . (flip elem addrChars)) r in
                     (address l u, xtra)]

ctoi = toEnum . fromEnum :: Char -> Int16

callPass :: Address -> Int16
callPass (Address a _) =
  0x7fff .&. (foldl xor 0x73e2 $ map (\(c, f) -> f (ctoi c)) $ zip a $ cycle [flip shiftL 8, id])

instance Similar Address where
  (≈) (Address a _) (Address b _) = a == b

newtype Body = Body String deriving (Eq)

instance Show Body where show (Body x) = x

data Position = Position { pos :: Geodetic WGS84, ambiguity :: Int }

position :: Body -> Maybe Position
position _ = Nothing

data Frame = Frame { source :: Address
                   , dest :: Address
                   , path :: [String]
                   , body :: Body }
           deriving (Eq)

instance Read Frame where
  readsPrec _ x = [let (addrd, msgd) = splitOn ':' x
                       (src, dest') = splitOn '>' addrd
                       (dest, paths) = splitOn ',' dest'
                       path = words $ map (\c -> if c == ',' then ' ' else c) paths in
                      (Frame { path = path,
                               dest = (read dest),
                               source = (read src),
                               body = Body msgd
                             }, "")]

instance Show Frame where
  show (Frame src dst path body) =
    (show src) ++ ">" ++ (show dst) ++ "," ++ (intercalate "," path) ++ ":" ++ (show body)

decodeBase91 all@(a:b:c:d) =
  foldl (\a (c, i) -> i * ((toEnum . fromEnum $ c) -33) + a) 0 $ zip all [91^x | x <- [3,2..0]]
decodeBase91 _ = 0
