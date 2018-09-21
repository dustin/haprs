{-# Language OverloadedStrings #-}

module Main where

import APRS

import Control.Concurrent (forkIO)
import Control.Concurrent.Broadcast ( Broadcast )
import Control.Concurrent.STM (newTChanIO)
import Control.Monad (forever, when, void)
import Data.Semigroup ((<>))
import Data.String (fromString)
import Data.Text (Text, stripEnd)
import Network (PortID(..), connectTo)
import Network.Socket (HostName)
import Network.Socket.Internal (PortNumber)
import Options.Applicative
import System.Console.ANSI
import System.IO (Handle, hClose, hPutStr, hGetLine)
import qualified Control.Concurrent.Broadcast as Broadcast
import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString.Char8 as B
import qualified Network.MQTT as MQTT

connect :: String -> String -> String -> String -> IO Handle
connect s c p f = let h = takeWhile (/= ':') s
                      pd = Service $ drop (length h + 1) s in
                    do
                      a <- connectTo h pd
                      let stuff = "user " ++ c ++ " pass " ++ p ++ " vers haprs 0.1 filter " ++ f ++ "\r\n"
                      print stuff
                      hPutStr a stuff
                      pure a

data Options = Options { optFilter :: String
                       , optServer :: String
                       , optCallsign :: String
                       , optCallpass :: String
                       -- mqtt options
                       , optTopic :: MQTT.Topic
                       , optHost :: HostName
                       , optPort :: PortNumber
                       , optUser :: Text
                       , optPass :: Text
                       , optClient :: Text
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
  <*> option str (long "mqttserver" <> showDefault <> value "localhost" <> help "mqtt host")
  <*> option auto (long "mqttport" <> showDefault <> value 1883 <> help "mqtt port")
  <*> option str (long "user" <> value "" <> help "mqtt username")
  <*> option str (long "pass" <> value "" <> help "mqtt password")
  <*> option str (long "client" <> value "aprsgate" <> help "mqtt client name")

doBody :: String -> Frame -> IO ()
doBody s f@(Frame _ _ _ (NotUnderstoodPacket _)) =
  colored Vivid Red (":( " ++ s ++ " ") >> colored Vivid Black (show f)
doBody s f = colored Vivid Black (":) " ++ s ++ " ") >> colored Vivid Green (show f)

colored :: ColorIntensity -> Color -> String -> IO ()
colored ci c s = do
  setSGR [SetColor Foreground ci c]
  putStrLn s
  setSGR [Reset]

-- Console logging thread.  This listens to broadcasts forever and logs them.
consLog :: Broadcast Msg -> IO ()
consLog b = forever (Broadcast.listen b >>= l)
  where l (Msg s (Right f)) = doBody s f
        l (Msg s (Left _)) = colored Vivid Red $ "error parsing frame: " ++ s

entry :: Broadcast Msg -> String -> IO ()
entry _ s@('#':_) = colored Vivid Black s
entry b s = Broadcast.signal b (Msg s $ A.parseOnly parseFrame (stripEnd . fromString $ s))

runMQTT :: Options -> Broadcast Msg -> IO ()
runMQTT opts b = do
  cmds <- MQTT.mkCommands
  pubChan <- newTChanIO
  let conf = (MQTT.defaultConfig cmds pubChan)
             { MQTT.cClean = False
             , MQTT.cClientID = optClient opts
             , MQTT.cHost = optHost opts
             , MQTT.cPort = optPort opts
             , MQTT.cUsername = nilly $ optUser opts
             , MQTT.cPassword = nilly $ optPass opts
             , MQTT.cKeepAlive = Just 10
             }

  _ <- forkIO $ forever $ do
    m <- Broadcast.listen b
    case m of
      (Msg _ (Left _)) -> undefined
      (Msg mt _) -> MQTT.publish conf MQTT.NoConfirm False (optTopic opts) (B.pack mt)

  terminated <- MQTT.run conf
  print terminated

  where nilly "" = Nothing
        nilly s = Just s


gate :: Options -> IO ()
gate opts = do
  putStrLn $ "gatin' " ++ optServer opts ++ show opts
  b <- Broadcast.new
  _ <- forkIO $ consLog b
  when (optHost opts /= "") $ void $ forkIO $ runMQTT opts b
  a <- connect (optServer opts) (optCallsign opts) (optCallpass opts) (optFilter opts)
  forever (hGetLine a >>= entry b)

main :: IO ()
main = gate =<< execParser opts
  where opts = info (options <**> helper)
          ( fullDesc <> progDesc "APRS gateway" )
