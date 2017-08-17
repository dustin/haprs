module APRS.Types
    ( PacketType
    , Address(..)
    , Frame
    , identifyPacket
    , callPass
    ) where

import Data.Bits
import Data.Int
import Data.List

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
  | Invalid Char deriving (Show, Eq)

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

data Address = Address { call :: String, ssid :: String }

instance Show Address where
  show a = call a ++ "-" ++ ssid a

callPass :: Address -> Int16
callPass a =
  let ctoi c = (toEnum (fromEnum c)) ::Int16 in
    0x7fff .&. (foldl xor 0x73e2 $ map (\(x, n) -> shiftL (ctoi x) n) (zip (call a) (cycle [8, 0])))

data Info = String deriving (Show)

data Frame = Frame { original :: String
                   , source :: Address
                   , dest :: Address
                   , path :: [Address]
                   , body :: Info } deriving (Show)
