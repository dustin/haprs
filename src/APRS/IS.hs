{-# LANGUAGE OverloadedStrings #-}

module APRS.IS where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Text (Text, pack)
import Data.Char (isSpace)
import Data.Int (Int16)
import qualified Data.Attoparsec.Text as A

import APRS.Types (Address, Position(..), PosExtension(..), parseAddr, callPass)

-- user mycall[-ss] pass passcode[ vers softwarename softwarevers[ UDP udpport][ servercommand]]

data Identification = ID {
  _idAddress :: Address
  , _idSW :: Maybe (Text, Text)
  , _idUDPPort :: Maybe Int16
  , _idFilter :: Text
  }
  deriving (Eq, Show)

parseIdentification :: A.Parser Identification
parseIdentification = do
  addr <- "user" >> A.skipSpace *> parseAddr
  _ <- A.skipSpace
  pass <- ("pass" >> A.skipSpace *> A.decimal) A.<?> "call pass"
  when (pass /= callPass addr) $ fail "invalid callpass"
  _ <- A.skipSpace
  sw <- Just <$> ("vers" >> parseVers) <|> pure Nothing
  _ <- A.skipSpace
  udp <- Just <$> ("UDP" >> A.skipSpace >> A.decimal) <|> pure Nothing
  _ <- A.skipSpace
  cmd <- A.takeText
  pure $ ID addr sw udp cmd

  where parseVers :: A.Parser (Text, Text)
        parseVers = do
          a <- word
          b <- word
          pure (a, b)
        word :: A.Parser Text
        word = do
          _ <- A.skipSpace
          w <- A.takeWhile (not.isSpace)
          pure w

newtype Filter = Filter [FilterItem]
               deriving (Eq, Show)

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
    deriving (Show, Eq)

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
      parts <- "s/" *> (A.many' (A.satisfy (`notElem` ['/', ' ']))) `A.sepBy` A.char '/'
      case parts ++ repeat "" of
        (a:b:c:_) -> pure $ SymbolFilter a b c
        _           -> fail "too many symbol filter parts"

    areaFilter = AreaFilter <$> ("a/" *> A.double)
                 <*> ("/" *> A.double)
                 <*> ("/" *> A.double)
                 <*> ("/" *> A.double)

    qconsFilter = do
      conses <- "q/" *> A.many' (A.satisfy (`notElem` ['/', ' ']))
      aye <- "/I" *> pure True <|> pure False
      pure $ QConsFilter conses aye

    wordlist = word `A.sepBy` A.char '/'

      where word :: A.Parser Text
            word = pack <$> A.many1 (A.satisfy $ A.inClass "[A-z0-9]")

    addrlist = parseAddr `A.sepBy1` A.char '/'
