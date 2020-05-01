{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           APRS
import           APRS.Types                   (unAddress)

import           Control.Concurrent           (forkIO)
import           Control.Concurrent.Broadcast (Broadcast)
import qualified Control.Concurrent.Broadcast as Broadcast
import           Control.Monad                (forever, void, when)
import qualified Data.Attoparsec.Text         as A
import qualified Data.ByteString.Char8        as BCS
import qualified Data.ByteString.Lazy.Char8   as B
import           Data.Conduit                 (runConduit, (.|))
import qualified Data.Conduit.Combinators     as C
import           Data.Conduit.Network         (appSink, appSource, clientSettings, runTCPClient)
import qualified Data.Conduit.Text            as CT
import           Data.Maybe                   (fromJust, isJust)
import           Data.String                  (fromString)
import           Data.Text                    (Text, isPrefixOf, stripEnd, unpack)
import           Network.MQTT.Client
import           Network.URI
import           Options.Applicative
import           System.Console.ANSI

data Options = Options { optFilter   :: String
                       , optServer   :: String
                       , optCallsign :: String
                       , optCallpass :: String
                       -- mqtt options
                       , optTopic    :: Text
                       , optMQTTURL  :: Maybe URI
                       } deriving (Show)

data Msg = Msg String (Either String Frame) deriving(Show)

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

runMQTT :: Options -> Broadcast Msg -> IO ()
runMQTT Options{..} b = do
  let uri = fromJust optMQTTURL
  mc <- connectURI mqttConfig{_connID=cid (uriFragment uri)} uri

  forever $ do
    m <- Broadcast.listen b
    case m of
      (Msg _ (Left _))                     -> undefined
      (Msg mt (Right (Frame src dst _ _))) -> publishq mc (mconcat [optTopic, "/", p src, "/", p dst]) (B.pack mt) False QoS1 []

  where cid ['#']    = "aprs-gate"
        cid ('#':xs) = xs
        cid _        = "aprs-gate"

        p :: Address -> Text
        p = fst . unAddress

parseHostPort :: String -> (BCS.ByteString, Int)
parseHostPort s = let h = takeWhile (/= ':') s
                      ps = drop (length h + 1) s in
                    (fromString h, read ps)

gate :: Options -> IO ()
gate opts@Options{..} = do
  putStrLn $ "gatin' " <> optServer <> show opts
  b <- Broadcast.new
  _ <- forkIO $ consLog b
  when (isJust optMQTTURL) $ void $ forkIO $ runMQTT opts b

  let (h,p) = parseHostPort optServer
  runTCPClient (clientSettings p h) (app b)

    where app b ad = do
            runConduit $
              appSource ad
              .| C.yieldMany (BCS.pack <$> ["user ", optCallsign,
                                            " pass ", optCallpass,
                                            " vers haprs 0.1 filter ", optFilter, "\r\n"])
              .| appSink ad

            runConduit $
              appSource ad
              .| CT.decode CT.utf8
              .| CT.lines
              .| C.mapM_ (en b)

          en :: Broadcast Msg -> Text -> IO ()
          en b t
            | "#" `isPrefixOf` t = colored Vivid Black (unpack t)
            | otherwise = Broadcast.signal b (Msg (unpack t) $ A.parseOnly parseFrame (stripEnd t))


main :: IO ()
main = gate =<< execParser opts
  where opts = info (options <**> helper)
          ( fullDesc <> progDesc "APRS gateway" )
