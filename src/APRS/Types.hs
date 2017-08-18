module APRS.Types
    ( PacketType
    , Address
    , address
    , Similar
    , (≈)
    , Frame
    , identifyPacket
    , callPass
    ) where

import Data.Bits
import Data.Int
import Data.List

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
  | Position
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
identifyPacket '@' = Position
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

instance Read Address where
  readsPrec _ x = [let (l, r) = splitAt (maybe (length x) id (elemIndex '-' x)) x
                       r' = (tail' r)
                       off = maybe (length x) id $ findIndex (\c -> not $ elem c addrChars) r'
                       (u, xtra) = splitAt off r' in
                     (address l u, xtra)]
                  where tail' [] = []
                        tail' (x:xs) = xs

ctoi = toEnum . fromEnum :: Char -> Int16

callPass :: Address -> Int16
callPass (Address a _) =
  0x7fff .&. (foldl xor 0x73e2 $ map (\(c, f) -> f (ctoi c)) $ zip a $ cycle [flip shiftL 8, id])

instance Similar Address where
  (≈) (Address a _) (Address b _) = a == b

data Info = String deriving (Show)

data Frame = Frame { original :: String
                   , source :: Address
                   , dest :: Address
                   , path :: [Address]
                   , body :: Info }
           deriving (Show)
