{-# LANGUAGE OverloadedStrings #-}

module APRS.IS
  (
    Identification(..)
  , Filter(..)
  , FilterItem(..)
  , parseIdentification
  , parseFilter
  , parseFilterItem
  , passFrame
  ) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Char (isSpace)
import Data.Functor (($>))
import Data.Int (Int16)
import Data.List (intercalate)
import Data.Maybe (isJust)
import Data.Semigroup ((<>))
import Data.Text (Text, pack, unpack, isPrefixOf)
import qualified Data.Attoparsec.Text as A

import APRS.Types (Address, Frame(..), Position(..), PosExtension(..), position,
                   elemish, APRSPacket(..), parseAddr, callPass, distance)

-- user mycall[-ss] pass passcode[ vers softwarename softwarevers[ UDP udpport][ servercommand]]

data Identification = ID {
  _idAddress :: Address
  , _idSW :: Maybe (Text, Text)
  , _idUDPPort :: Maybe Int16
  , _idFilter :: Text
  , _idValidMsg :: Frame -> Bool
  }

instance Show Identification where
  show (ID a s u f _) = "ID{addr=" <> show a
                        <> ", sw=" <> show s
                        <> ", udp=" <> show u
                        <> ", cmd=" <> show f <> "}"

instance Eq Identification where
  (ID a s u f _) == (ID a' s' u' f' _) = a == a' && s == s' && u == u' && f == f'

parseIdentification :: A.Parser Identification
parseIdentification = do
  addr <- "user" >> A.skipSpace *> parseAddr
  _ <- A.skipSpace
  pass <- ("pass" >> A.skipSpace *> A.signed A.decimal) A.<?> "call pass"
  when (pass /= (-1) && pass /= callPass addr) $ fail "invalid callpass"
  _ <- A.skipSpace
  sw <- Just <$> ("vers" >> parseVers) <|> pure Nothing
  _ <- A.skipSpace
  udp <- Just <$> ("UDP" >> A.skipSpace >> A.decimal) <|> pure Nothing
  _ <- A.skipSpace
  cmd <- A.takeText
  pure $ ID addr sw udp cmd (const (pass /= (-1)))

  where parseVers :: A.Parser (Text, Text)
        parseVers = do
          a <- word
          b <- word
          pure (a, b)
        word :: A.Parser Text
        word = A.skipSpace *> A.takeWhile (not.isSpace)

newtype Filter = Filter [FilterItem]
               deriving (Eq)

instance Show Filter where
  show (Filter i) = "filter " <> intercalate " " (map show i)

parseFilter :: A.Parser Filter
parseFilter = do
  _ <- A.skipSpace
  Filter <$> ("filter" >> A.skipSpace *> A.many1 (A.skipSpace >> parseFilterItem))

data FilterItem =
  NotFilter FilterItem
  | RangeFilter Position Double
  | PrefixFilter [Text]
  | BudlistFilter [Address]
  | ObjectFilter [Text]
  | StrictObjectFilter [Text]
  | TypeFilter [Char] (Maybe (Address, Double))
  | SymbolFilter [Char] [Char] [Char]
  | DigiFilter [Address]
  | AreaFilter Double Double Double Double
  | EntryStationFilter [Address]
  | GroupMessageFilter [Address]
  | UnprotoFilter [Address]
  | QConsFilter [Char] Bool
  | MyRangeFilter Double
  | FriendRangeFilter Address Double
    deriving (Eq)

instance Show FilterItem where
  show (NotFilter x) = "-" <> show x
  show (RangeFilter (Position (lon,lat,_,_)) d) = "r/" <> show lat <> "/" <> show lon <> "/" <> show d
  show (PrefixFilter p) = "p/" <> intercalate "/" (map unpack p)
  show (BudlistFilter b) = "b/" <> intercalate "/" (map show b)
  show (ObjectFilter b) = "o/" <> intercalate "/" (map unpack b)
  show (StrictObjectFilter b) = "os/" <> intercalate "/" (map unpack b)
  show (TypeFilter t Nothing) = "t/" <> t
  show (TypeFilter t (Just (c,d))) = "t/" <> t <> "/" <> show c <> "/" <> show d
  show (SymbolFilter a "" "") = "s/" <> a
  show (SymbolFilter a b "") = "s/" <> intercalate "/" [a, b]
  show (SymbolFilter a b c) = "s/" <> intercalate "/" [a, b, c]
  show (DigiFilter a) = "d/" <> intercalate "/" (map show a)
  show (AreaFilter a b c d) = "a/" <> intercalate "/" (map show [a, b, c, d])
  show (EntryStationFilter a) = "e/" <> intercalate "/" (map show a)
  show (GroupMessageFilter a) = "g/" <> intercalate "/" (map show a)
  show (UnprotoFilter a) = "u/" <> intercalate "/" (map show a)
  show (QConsFilter x False) = "q/" <> x
  show (QConsFilter x True) = "q/" <> x <> "/I"
  show (MyRangeFilter d) = "m/" <> show d
  show (FriendRangeFilter a d) = "f/" <> show a <> "/" <> show d

-- TODO(dustin): Wildcard support is a thing.

matchesTypeFilter :: Char -> Frame -> Bool
matchesTypeFilter 'p' frame = (isJust.position) frame
matchesTypeFilter 'o' (Frame _ _ _ ObjectPacket{}) = True
matchesTypeFilter 'i' (Frame _ _ _ ItemPacket{}) = True
matchesTypeFilter 'm' (Frame _ _ _ MessagePacket{}) = True
matchesTypeFilter 'q' _ = undefined -- query
matchesTypeFilter 's' (Frame _ _ _ StatusPacket{}) = True
matchesTypeFilter 't' (Frame _ _ _ TelemetryPacket{}) = True
matchesTypeFilter 'u' _ = undefined -- user defined
matchesTypeFilter 'n' (Frame _ _ _ WeatherPacket{}) = True -- nws
matchesTypeFilter 'w' (Frame _ _ _ WeatherPacket{}) = True
matchesTypeFilter _ _ = False


passFrame :: FilterItem -> Frame -> Bool
passFrame (NotFilter x) frame = (not.passFrame x) frame
passFrame (RangeFilter p d) frame = case position frame of
                                      Nothing -> False
                                      Just p2 -> distance p p2 <= d
passFrame (PrefixFilter p) (Frame src _ _ _) = let s = (pack.show) src in
                                                 any (`isPrefixOf` s) p
passFrame (BudlistFilter b) (Frame src _ _ _) = src `elemish` b
passFrame (ObjectFilter b) (Frame _ _ _ (ObjectPacket _ _ n _ _ _)) = n `elem` b
passFrame (StrictObjectFilter b) frame = passFrame (ObjectFilter b) frame -- XXX: ???
passFrame (TypeFilter t Nothing) frame = any (`matchesTypeFilter` frame) t
passFrame (TypeFilter _t (Just (_c,_d))) _frame = undefined
passFrame (SymbolFilter _pri "" "") _frame = undefined
passFrame (SymbolFilter _pri _sec "") _frame = undefined
passFrame (SymbolFilter _a _b _c) _frame = undefined
passFrame (DigiFilter _a) _frame = undefined
passFrame (AreaFilter latN lonW latS lonE) frame = case position frame of
                                                     Nothing -> False
                                                     Just (Position (lon,lat,_,_)) -> and [
                                                       latS <= lat, latN >= lat,
                                                       lonW <= lon, lonE >= lon
                                                       ]
passFrame (EntryStationFilter _a) _frame = undefined
passFrame (GroupMessageFilter a) (Frame _ dst _ _) = dst `elemish` a
passFrame (UnprotoFilter _a) _frame = undefined
passFrame (QConsFilter _x False) _frame = undefined
passFrame (QConsFilter _x True) _frame = undefined
passFrame (MyRangeFilter _d) _frame = undefined
passFrame (FriendRangeFilter _a _d) _frame = undefined
passFrame _ _ = False

parseFilterItem :: A.Parser FilterItem
parseFilterItem = NotFilter <$> ("-" *> parseFilterItem)
                  <|> rangeFilter
                  <|> PrefixFilter <$> ("p/" *> wordlist)
                  <|> BudlistFilter <$> ("b/" *> addrlist)
                  <|> ObjectFilter <$> ("o/" *> wordlist)
                  <|> StrictObjectFilter <$> ("os/" *> wordlist)
                  <|> typeFilter
                  <|> symFilter
                  <|> DigiFilter <$> ("d/" *> addrlist)
                  <|> areaFilter
                  <|> EntryStationFilter <$> ("e/" *> addrlist)
                  <|> GroupMessageFilter <$> ("g/" *> addrlist)
                  <|> UnprotoFilter <$> ("u/" *> addrlist)
                  <|> qconsFilter
                  <|> MyRangeFilter <$> ("m/" *> A.double)
                  <|> FriendRangeFilter <$> ("f/" *> parseAddr) <*> ("/" *> A.double)


  where
    rangeFilter = do
      [lat,lon,dist] <- "r/" *> A.double `A.sepBy` A.char '/'
      pure $ RangeFilter (Position (lon, lat, 0, PosENone)) dist

    typeFilter = do
      stuff <- "t/" *> A.many1 (A.satisfy (`elem` ("poimqstunw" :: [Char])))
      calldist <- (Just <$> cd) <|> pure Nothing
      pure $ TypeFilter stuff calldist

      where cd :: A.Parser (Address, Double)
            cd = do
              a <- "/" *> parseAddr
              d <- "/" *> A.double
              pure (a, d)

    symFilter = do
      parts <- "s/" *> A.many' (A.satisfy (`notElem` ['/', ' '])) `A.sepBy` A.char '/'
      let (a:b:c:_) = parts ++ repeat "" in pure $ SymbolFilter a b c

  -- a/latN/lonW/latS/lonE
    areaFilter = do
      af@(AreaFilter latN lonW latS lonE) <- AreaFilter <$> ("a/" *> A.double)
                                             <*> ("/" *> A.double)
                                             <*> ("/" *> A.double)
                                             <*> ("/" *> A.double)
      when (latN < latS) $ fail "north must be >= south"
      when (lonE < lonW) $ fail "east must be >= west"
      pure af

    qconsFilter = do
      conses <- "q/" *> A.many' (A.satisfy (`notElem` ['/', ' ']))
      aye <- "/I" $> True <|> pure False
      pure $ QConsFilter conses aye

    wordlist = word `A.sepBy` A.char '/'

      where word :: A.Parser Text
            word = pack <$> A.many1 (A.satisfy $ A.inClass "[A-z0-9]")

    addrlist = parseAddr `A.sepBy1` A.char '/'
