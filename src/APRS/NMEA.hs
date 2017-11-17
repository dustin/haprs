{-# LANGUAGE OverloadedStrings #-}

module APRS.NMEA where

{-
GGA - essential fix data which provide 3D location and accuracy data.

 $GPGGA,123519,4807.038,N,01131.000,E,1,08,0.9,545.4,M,46.9,M,,*47

Where:
     GGA          Global Positioning System Fix Data
     123519       Fix taken at 12:35:19 UTC
     4807.038,N   Latitude 48 deg 07.038' N
     01131.000,E  Longitude 11 deg 31.000' E
     1            Fix quality: 0 = invalid
                               1 = GPS fix (SPS)
                               2 = DGPS fix
                               3 = PPS fix
			       4 = Real Time Kinematic
			       5 = Float RTK
                               6 = estimated (dead reckoning) (2.3 feature)
			       7 = Manual input mode
			       8 = Simulation mode
     08           Number of satellites being tracked
     0.9          Horizontal dilution of position
     545.4,M      Altitude, Meters, above mean sea level
     46.9,M       Height of geoid (mean sea level) above WGS84
                      ellipsoid
     (empty field) time in seconds since last DGPS update
     (empty field) DGPS station ID number
     *47          the checksum data, always begins with *
-}

import Control.Applicative ((<|>))
import Control.Monad (replicateM)
import qualified Data.Attoparsec.Text as A

-- Lat, Long, (H,M,S)
parseNMEA :: A.Parser (Double, Double, (Int, Int, Int))
parseNMEA = parseGGA <|> parseRMC

parseGGA :: A.Parser (Double, Double, (Int, Int, Int))
parseGGA = do
  _ <- A.string "$GPGGA,"
  ts <- parseTS
  _ <- A.char ','
  lat <- parseDir 2
  _ <- A.char ','
  lon <- parseDir 3

  return (lat, lon, ts)

parseTS :: A.Parser (Int, Int, Int)
parseTS = do
  digs <- replicateM 3 (replicateM 2 A.digit)
  let [h,m,s] = map read digs
  return (h, m, s)

parseDir :: Int -> A.Parser Double
parseDir n = do
  cD <- replicateM n A.digit
  cM <- A.double
  _ <- A.char ','
  cS <- A.satisfy (`elem` ['N', 'E', 'W', 'S'])
  return $ (read cD + (cM / 60)) * (if cS `elem` ['W', 'S'] then -1 else 1)

{-
$GPRMC,123519,A,4807.038,N,01131.000,E,022.4,084.4,230394,003.1,W*6A

Where:
     RMC          Recommended Minimum sentence C
     123519       Fix taken at 12:35:19 UTC
     A            Status A=active or V=Void.
     4807.038,N   Latitude 48 deg 07.038' N
     01131.000,E  Longitude 11 deg 31.000' E
     022.4        Speed over the ground in knots
     084.4        Track angle in degrees True
     230394       Date - 23rd of March 1994
     003.1,W      Magnetic Variation
     *6A          The checksum data, always begins with *
-}

parseRMC :: A.Parser (Double, Double, (Int, Int, Int))
parseRMC = do
  _ <- A.string "$GPRMC,"
  ts <- parseTS
  _ <- A.string ",A,"
  lat <- parseDir 2
  _ <- A.char ','
  lon <- parseDir 3

  return (lat, lon, ts)
