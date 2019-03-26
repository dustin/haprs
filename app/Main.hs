{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           APRS

import           Control.Concurrent           (forkIO)
import           Control.Concurrent.Broadcast (Broadcast)
import qualified Control.Concurrent.Broadcast as Broadcast
import           Control.Monad                (forever, void, when)
import qualified Data.Attoparsec.Text         as A
import qualified Data.ByteString.Lazy.Char8   as B
import           Data.Maybe                   (fromJust, isJust)
import           Data.Semigroup               ((<>))
import           Data.String                  (fromString)
import           Data.Text                    (Text, pack, stripEnd)
import           Network                      (PortID (..), connectTo)
import           Network.MQTT.Client
import           Network.URI
import           Options.Applicative
import           System.Console.ANSI
import           System.IO                    (Handle, hGetLine, hPutStr)

connect :: String -> String -> String -> String -> IO Handle
connect s c p f = let h = takeWhile (/= ':') s
                      pd = Service $ drop (length h + 1) s in
                    do
                      a <- connectTo h pd
                      let stuff = mconcat ["user ", c, " pass ", p, " vers haprs 0.1 filter ", f, "\r\n"]
                      print stuff
                      hPutStr a stuff
                      pure a

data Options = Options { optFilter   :: String
                       , optServer   :: String
                       , optCallsign :: String
                       , optCallpass :: String
                       -- mqtt options
                       , optTopic    :: Text
                       , optMQTTURL  :: Maybe URI
                       } deriving (Show)

data Msg = Msg String (Either String Frame)

options :: Parser Options
options = Options
  <$> strOption (long "filter" <> showDefault <> value "" <> help "APRS filter")
  <*> strOption (long "server" <> showDefault <> value "rotate.aprs2.net:14580" <> help "APRS-IS server")
  <*> strOption (long "callsign" <> showDefault <> value "" <> help "your callsign")
  <*> strOption (long "callpass" <> showDefault <> value "" <> help "your callpass")
  <*> option str (long "topic" <> showDefault <> value "aprs" <>
                  help "mqtt topic - if ends with a slash, sensor serial number will be appended")
  <*> option (maybeReader $ pure . parseURI) (long "mqtt-uri" <> showDefault <> value (parseURI "mqtt://test.mosquitto.org/#aprs-gate") <> help "mqtt broker URI")

doBody :: String -> Frame -> IO ()
doBody s f@(Frame _ _ _ (NotUnderstoodPacket _)) =
  colored Vivid Red (":( " <> s <> " ") >> colored Vivid Black (show f)
doBody s f = colored Vivid Black (":) " <> s <> " ") >> colored Vivid Green (show f)

colored :: ColorIntensity -> Color -> String -> IO ()
colored ci c s = do
  setSGR [SetColor Foreground ci c]
  putStrLn s
  setSGR [Reset]

-- Console logging thread.  This listens to broadcasts forever and logs them.
consLog :: Broadcast Msg -> IO ()
consLog b = forever (Broadcast.listen b >>= l)
  where l (Msg s (Right f)) = doBody s f
        l (Msg s (Left _))  = colored Vivid Red $ "error parsing frame: " <> s

entry :: Broadcast Msg -> String -> IO ()
entry _ s@('#':_) = colored Vivid Black s
entry b s = Broadcast.signal b (Msg s $ A.parseOnly parseFrame (stripEnd . fromString $ s))

runMQTT :: Options -> Broadcast Msg -> IO ()
runMQTT Options{..} b = do
  let uri = fromJust optMQTTURL
  mc <- connectURI mqttConfig{_connID=cid (uriFragment uri)} uri

  forever $ do
    m <- Broadcast.listen b
    case m of
      (Msg _ (Left _))                     -> undefined
      (Msg mt (Right (Frame src dst _ _))) -> publishq mc (mconcat [optTopic, "/", p src, "/", p dst]) (B.pack mt) False QoS1

  where cid ['#']    = "aprs-gate"
        cid ('#':xs) = xs
        cid _        = "aprs-gate"

        p :: Show a => a -> Text
        p = pack . show

gate :: Options -> IO ()
gate opts = do
  putStrLn $ "gatin' " <> optServer opts <> show opts
  b <- Broadcast.new
  _ <- forkIO $ consLog b
  when (isJust $ optMQTTURL opts) $ void $ forkIO $ runMQTT opts b
  a <- connect (optServer opts) (optCallsign opts) (optCallpass opts) (optFilter opts)
  forever (hGetLine a >>= entry b)

main :: IO ()
main = gate =<< execParser opts
  where opts = info (options <**> helper)
          ( fullDesc <> progDesc "APRS gateway" )
