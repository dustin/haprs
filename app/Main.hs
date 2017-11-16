module Main where

import APRS.Types

import Control.Monad (forever)
import System.IO (Handle, hClose, hPutStr, hGetLine)
import Network (PortID(..), connectTo)
import Data.String (fromString)
import Options.Applicative
import Data.Text (stripEnd)
import Data.Semigroup ((<>))

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

options :: Parser Options
options = Options
  <$> strOption (long "filter" <> showDefault <> value "" <> help "APRS filter")
  <*> strOption (long "server" <> showDefault <> value "rotate.aprs2.net:14580" <> help "APRS-IS server")
  <*> strOption (long "callsign" <> showDefault <> value "" <> help "your callsign")
  <*> strOption (long "callpass" <> showDefault <> value "" <> help "your callpass")

doBody :: String -> Frame -> IO ()
doBody s f@(Frame _ _ _ (NotImplemented _ _)) =
  colored Vivid Black (":( " ++ s ++ " ") >> colored Vivid Yellow (show f)
doBody s f@(Frame _ _ _ (GarbagePacket _)) =
  colored Vivid Red (":( " ++ s ++ " ") >> colored Vivid Black (show f)
doBody s f = colored Vivid Black (":) " ++ s ++ " ") >> colored Vivid Green (show f)

colored :: ColorIntensity -> Color -> String -> IO ()
colored ci c s = do
  setSGR [SetColor Foreground ci c]
  putStrLn s
  setSGR [Reset]

entry :: String -> IO ()
entry s@('#':_) =
  do colored Vivid Black s
entry s = do
  case A.parseOnly parseFrame (stripEnd . fromString $ s) of
    Right f -> doBody s f
    _ -> colored Vivid Red $ "error parsing frame: " ++ s

gate :: Options -> IO ()
gate opts@(Options oFilt oSvr oCall oPass) = do
  putStrLn $ "gatin' " ++ oSvr ++ show opts
  a <- connect oSvr oCall oPass oFilt
  forever (hGetLine a >>= entry)

main :: IO ()
main = gate =<< execParser opts
  where opts = info (options <**> helper)
          ( fullDesc <> progDesc "APRS gateway" )
