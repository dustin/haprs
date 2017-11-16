module APRS.MicE where

import Data.Text (Text)

{-
The Destination Address field contains:
• Six encoded latitude digits specifying degrees (digits 1 and 2), minutes
  (digits 3 and 4) and hundredths of minutes (digits 5 and 6).
• 3-bit Mic-E message identifier (message bits A, B and C).
• North/South latitude indicator.
• Longitude offset (adds 0 degrees or 100 degrees to the longitude
  computation in the Information field).
• West/East longitude indicator.
• Generic APRS digipeater path (encoded in the SSID).
-}

-- In:  Call, SSID
-- Out: Lat, MID, lon offset, Lon Sign, digipeater path
parseMicEDest :: Text -> Text -> (Double, Int, Int, Int, Int)
parseMicEDest call ssid = undefined

micELonD :: Char -> Int -> Int
micELonD c off
  | between 180 d 189 = d - 80
  | between 190 d 199 = d - 190
  | otherwise = d

  where d = off + (fromEnum c) - 28
        between l v h = v >= l && v <= h

micELonM :: Char -> Int
micELonM c = ((fromEnum c) - 28) `mod` 60

