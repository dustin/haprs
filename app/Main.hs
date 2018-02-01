module Main where

import APRS

import Control.Monad (forever)
import System.IO (Handle, hClose, hPutStr, hGetLine)
import Network (PortID(..), connectTo)
import Data.String (fromString)
import Options.Applicative
import Data.Text (stripEnd)
import Data.Semigroup ((<>))

import Control.Concurrent (forkIO)
import Control.Concurrent.Broadcast ( Broadcast )
import qualified Control.Concurrent.Broadcast as Broadcast


import qualified Data.Attoparsec.Text as A

import System.Console.ANSI

connect :: String -> String -> String -> String -> IO Handle
connect s c p f = let h = takeWhile (/= ':') s
                      pd = Service $ drop (length h + 1) s in
                    do
                      a <- connectTo h pd
                      let stuff = "user " ++ c ++ " pass " ++ p ++ " vers haprs 0.1 filter " ++ f ++ "\r\n"
                      print stuff
                      hPutStr a stuff
                      return a

data Options = Options { optFilter :: String
                       , optServer :: String
                       , optCallsign :: String
                       , optCallpass :: String
                       } deriving (Show)

data Msg = Msg String (Either String Frame)

options :: Parser Options
options = Options
  <$> strOption (long "filter" <> showDefault <> value "" <> help "APRS filter")
  <*> strOption (long "server" <> showDefault <> value "rotate.aprs2.net:14580" <> help "APRS-IS server")
  <*> strOption (long "callsign" <> showDefault <> value "" <> help "your callsign")
  <*> strOption (long "callpass" <> showDefault <> value "" <> help "your callpass")

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

gate :: Options -> IO ()
gate opts@(Options oFilt oSvr oCall oPass) = do
  putStrLn $ "gatin' " ++ oSvr ++ show opts
  b <- Broadcast.new
  _ <- forkIO $ consLog b
  a <- connect oSvr oCall oPass oFilt
  forever (hGetLine a >>= entry b)

main :: IO ()
main = gate =<< execParser opts
  where opts = info (options <**> helper)
          ( fullDesc <> progDesc "APRS gateway" )
