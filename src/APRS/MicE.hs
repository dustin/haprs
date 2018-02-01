module APRS.MicE where

import Data.Text (Text, unpack)

-- In:  Call, SSID
-- Out: Lat, MID, lon offset, Lon Sign, digipeater path
micEDest :: Text -> Text -> (Double, Int, Int, Int, Int)
micEDest t _ssid =
  let [a,b,c,d,e,f] = unpack t
      deg = (digit a * 10 + digit b) +
            ((digit c * 10 + digit d + digit e / 10 + digit f / 100) / 60) in
    (deg * sign d, bit a * 4 + bit b * 2 + bit c, offset e, (negate.sign) f, 0)

  where
    sign c = if c < 'P' then (-1) else 1
    offset c = if c < 'P' then 0 else 100
    bit c = if (c >= 'A' && c <= 'K') || (c > 'P') then 1 else 0

digit :: Char -> Double
digit c
  | c >= '0' && c <= '9' = fromIntegral $ fromEnum c - 48
  | c >= 'A' && c <= 'J' = fromIntegral $ fromEnum c - 65
  | c >= 'P' && c <= 'Y' = fromIntegral $ fromEnum c - 80
  | otherwise = 0

micELonD :: Char -> Int -> Int
micELonD c off
  | between 180 d 189 = d - 80
  | between 190 d 199 = d - 190
  | otherwise = d

  where d = off + fromEnum c - 28
        between l v h = v >= l && v <= h

micELonM :: Char -> Int
micELonM c = (fromEnum c - 28) `mod` 60
