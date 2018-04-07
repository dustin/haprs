{-# LANGUAGE OverloadedStrings #-}

module APRS.IS where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Text (Text)
import Data.Char (isSpace)
import Data.Int (Int16)
import Text.Read (readMaybe)
import Numeric (readInt)
import qualified Data.Attoparsec.Text as A

import APRS.Types (Address, parseAddr, callPass)

-- user mycall[-ss] pass passcode[ vers softwarename softwarevers[ UDP udpport][ servercommand]]

data Identification = ID {
  _idAddress :: Address
  , _idSW :: Maybe (Text, Text)
  , _idUDPPort :: Maybe Int16
  , _idCmd :: Text
  }
  deriving (Eq, Show)

parseIdentification :: A.Parser Identification
parseIdentification = do
  addr <- "user" >> A.skipSpace *> parseAddr
  _ <- A.skipSpace
  pass <- ("pass" >> A.skipSpace *> A.decimal) A.<?> "call pass"
  when (pass /= callPass addr) $ fail "invalid callpass"
  _ <- A.skipSpace
  sw <- ("vers" >> parseVers >>= pure.Just) <|> pure Nothing
  _ <- A.skipSpace
  udp <- ("UDP" >> A.skipSpace >> A.decimal >>= pure.Just) <|> pure Nothing
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
