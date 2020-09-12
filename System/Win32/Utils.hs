{- |
   Module      :  System.Win32.Utils
   Copyright   :  2009 Balazs Komuves, 2013 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Utilities for calling Win32 API
-}
module System.Win32.Utils
  ( try, tryWithoutNull, trySized, try'
  -- * Maybe values
  , maybePtr, ptrToMaybe, maybeNum, numToMaybe
  , peekMaybe, withMaybe
  -- * Format picture translation
  , fromDateFormatPicture
  , fromTimeFormatPicture
  ) where

import Control.Monad               ( unless )
import Foreign.C.Types             ( CInt )
import Foreign.Marshal.Array       ( allocaArray, peekArray )
import Foreign.Marshal.Utils       ( with )
import Foreign.Ptr                 ( Ptr, nullPtr )
import Foreign.Storable            ( Storable(..) )
import Text.ParserCombinators.ReadP ( ReadP, (<++), between, char, count
                                    , readP_to_S, satisfy )


import System.Win32.String         ( LPTSTR, peekTString, peekTStringLen
                                   , withTStringBufferLen )
import System.Win32.Types          ( BOOL, UINT, eRROR_INSUFFICIENT_BUFFER
                                   , failIfZero, failWith, getLastError
                                   , maybeNum, maybePtr, numToMaybe
                                   , ptrToMaybe )
import qualified System.Win32.Types ( try )
import System.Win32.Word           ( DWORD, PDWORD )

-- | Support for API calls that are passed a fixed-size buffer and tell
-- you via the return value if the buffer was too small.  In that
-- case, we extend the buffer size and try again.
try :: String -> (LPTSTR -> UINT -> IO UINT) -> UINT -> IO String
try = System.Win32.Types.try
{-# INLINE try #-}

tryWithoutNull :: String -> (LPTSTR -> UINT -> IO UINT) -> UINT -> IO String
tryWithoutNull loc f n = do
   e <- allocaArray (fromIntegral n) $ \lptstr -> do
          r <- failIfZero loc $ f lptstr n
          if r > n then return (Left r) else do
            str <- peekTString lptstr
            return (Right str)
   case e of
        Left r'   -> tryWithoutNull loc f r'
        Right str -> return str

try' :: Storable a => String -> (Ptr a -> PDWORD -> IO BOOL) -> DWORD -> IO [a]
try' loc f n =
   with n $ \n' -> do
   e <- allocaArray (fromIntegral n) $ \lptstr -> do
          flg <- f lptstr n'
          unless flg $ do
            err_code <- getLastError
            unless (err_code == eRROR_INSUFFICIENT_BUFFER)
              $ failWith loc err_code
          r   <- peek n'
          if r > n then return (Left r) else do
            str <- peekArray (fromIntegral r) lptstr
            return (Right str)
   case e of
        Left r'   -> try' loc f r'
        Right str -> return str

-- | Support for API calls that return the required size, in characters
-- including a null character, of the buffer when passed a buffer size of zero.
trySized :: String -> (LPTSTR -> CInt -> IO CInt) -> IO String
trySized wh f = do
    c_len <- failIfZero wh $ f nullPtr 0
    let len = fromIntegral c_len
    withTStringBufferLen len $ \(buf', len') -> do
        let c_len' = fromIntegral len'
        c_len'' <- failIfZero wh $ f buf' c_len'
        let len'' = fromIntegral c_len''
        peekTStringLen (buf', len'' - 1) -- Drop final null character

-- | See also: 'Foreign.Marshal.Utils.maybePeek' function.
peekMaybe :: Storable a => Ptr a -> IO (Maybe a)
peekMaybe p =
  if p == nullPtr
    then return Nothing
    else Just `fmap` peek p

-- | See also: 'Foreign.Marshal.Utils.maybeWith' function.
withMaybe :: Storable a => Maybe a -> (Ptr a -> IO b) -> IO b
withMaybe Nothing  action = action nullPtr
withMaybe (Just x) action = with x action

-- | Type representing components of a Windows API day, month, year and era
-- format picture.
data DateFormatPicture
  = Day
  | Day0 -- Padded with zeros
  | DayShort
  | DayLong
  | Month
  | Month0 -- Padded with zeros
  | MonthShort
  | MonthLong
  | YearVeryShort -- Year represented only by the last digit
  | YearShort
  | Year
  | Era
  | DateOther String
  deriving (Eq, Show)

fromDFP :: DateFormatPicture -> String
fromDFP Day = "%-e" -- No padding
fromDFP Day0 = "%d" -- Padded with zeros
fromDFP DayShort = "%a" -- eg Tue
fromDFP DayLong = "%A" -- eg Tuesday
fromDFP Month = "%-m" -- No padding
fromDFP Month0 = "%m" -- Padded with zeros
fromDFP MonthShort = "%b" -- eg Jan
fromDFP MonthLong = "%B" -- eg January
fromDFP YearVeryShort = "%-y" -- No direct equivalent of a one digit year, so
                              -- do not distinguish from a short year without
                              -- padding
fromDFP YearShort = "%y"
fromDFP Year = "%Y"
fromDFP Era = "" -- No equivalent
fromDFP (DateOther cs) = escape cs

escape :: String -> String
escape [] = []
escape (c:cs) = escape' c ++ escape cs
 where
  escape' '%' = "%%"
  escape' '\t' = "%t"
  escape' '\n' = "%n"
  escape' c' = [c']

d :: ReadP Char
d = char 'd'

day :: ReadP DateFormatPicture
day = do
  _ <- d
  return Day

day0 :: ReadP DateFormatPicture
day0 = do
  _ <- count 2 d
  return Day0

dayShort :: ReadP DateFormatPicture
dayShort = do
  _ <- count 3 d
  return DayShort

dayLong :: ReadP DateFormatPicture
dayLong = do
  _ <- count 4 d
  return DayLong

days :: ReadP DateFormatPicture
days = dayLong <++ dayShort <++ day0 <++ day

bigM :: ReadP Char
bigM = char 'M'

month :: ReadP DateFormatPicture
month = do
  _ <- bigM
  return Month

month0 :: ReadP DateFormatPicture
month0 = do
  _ <- count 2 bigM
  return Month0

monthShort :: ReadP DateFormatPicture
monthShort = do
  _ <- count 3 bigM
  return MonthShort

monthLong :: ReadP DateFormatPicture
monthLong = do
  _ <- count 4 bigM
  return MonthLong

months :: ReadP DateFormatPicture
months = monthLong <++ monthShort <++ month0 <++ month

y :: ReadP Char
y = char 'y'

yearVeryShort :: ReadP DateFormatPicture
yearVeryShort = do
  _ <- y
  return YearVeryShort

yearShort :: ReadP DateFormatPicture
yearShort = do
  _ <- count 2 y
  return YearShort

year :: ReadP DateFormatPicture
year = do
  _ <- count 5 y <++ count 4 y
  return Year

years :: ReadP DateFormatPicture
years = year <++ yearShort <++ yearVeryShort

g :: ReadP Char
g = char 'g'

era :: ReadP DateFormatPicture
era = do
  _ <- count 2 g <++ count 1 g
  return Era

quote :: ReadP Char
quote = char '\''

notQuote :: ReadP Char
notQuote = satisfy (/= '\'')

escQuote :: ReadP Char
escQuote = do
  _ <- count 2 quote
  return '\''

quotedChars :: ReadP String
quotedChars = between quote quote $ greedy (escQuote <++ notQuote)

-- | Although not documented at
-- https://docs.microsoft.com/en-us/windows/win32/intl/day--month--year--and-era-format-pictures
-- the format pictures used by Windows do not require all such characters to be
-- enclosed in single quotation marks.
nonDateSpecial :: ReadP Char
nonDateSpecial = satisfy (\c -> c `notElem` ['d', 'M', 'y', 'g', '\''])

nonDateSpecials :: ReadP String
nonDateSpecials = greedy1 nonDateSpecial

dateOther :: ReadP DateFormatPicture
dateOther = do
  chars <- greedy1 (nonDateSpecials <++ quotedChars)
  return $ DateOther $ concat chars

datePicture :: ReadP [DateFormatPicture]
datePicture = greedy (days <++ months <++ years <++ era <++ dateOther)

-- | Type representing components of a Windows API hours, minute, and second
-- format picture.
data TimeFormatPicture
  = Hours12
  | Hours012 -- Padded with zeros
  | Hours24
  | Hours024 -- Padded with zeros
  | Minutes
  | Minutes0 -- Padded with zeros
  | Seconds
  | Seconds0 -- Padded with zeros
  | TimeMarkerShort -- One-character time marker string, eg "A" and "P"
  | TimeMarker -- Multi-character time marker string, eg "AM" and "PM"
  | TimeOther String
  deriving (Eq, Show)

fromTFP :: TimeFormatPicture -> String
fromTFP Hours12 = "%-l" -- No padding
fromTFP Hours012 = "%I" -- Padded with zeros
fromTFP Hours24 = "%-k" -- No padding
fromTFP Hours024 = "%H" -- Padded with zeros
fromTFP Minutes = "%-M" -- No padding
fromTFP Minutes0 = "%M" -- Padded with zeros
fromTFP Seconds = "%-S" -- No padding
fromTFP Seconds0 = "%S" -- Padded with zeros
fromTFP TimeMarkerShort = "%p" -- No direct equivalent, so do not distinguish
                               -- from TimeMarker
fromTFP TimeMarker = "%p"
fromTFP (TimeOther cs) = escape cs

h :: ReadP Char
h = char 'h'

hours12 :: ReadP TimeFormatPicture
hours12 = do
  _ <- h
  return Hours12

hours012 :: ReadP TimeFormatPicture
hours012 = do
  _ <- count 2 h
  return Hours012

bigH :: ReadP Char
bigH = char 'H'

hours24 :: ReadP TimeFormatPicture
hours24 = do
  _ <- bigH
  return Hours24

hours024 :: ReadP TimeFormatPicture
hours024 = do
  _ <- count 2 bigH
  return Hours024

hours :: ReadP TimeFormatPicture
hours = hours012 <++ hours12 <++ hours024 <++ hours24

m :: ReadP Char
m = char 'm'

minute :: ReadP TimeFormatPicture
minute = do
  _ <- m
  return Minutes

minute0 :: ReadP TimeFormatPicture
minute0 = do
  _ <- count 2 m
  return Minutes0

minutes :: ReadP TimeFormatPicture
minutes = minute0 <++ minute

s :: ReadP Char
s = char 's'

second :: ReadP TimeFormatPicture
second = do
  _ <- s
  return Seconds

second0 :: ReadP TimeFormatPicture
second0 = do
  _ <- count 2 s
  return Seconds0

seconds :: ReadP TimeFormatPicture
seconds = second0 <++ second

t :: ReadP Char
t = char 't'

timeMarkerShort :: ReadP TimeFormatPicture
timeMarkerShort = do
  _ <- t
  return TimeMarkerShort

timeMarker :: ReadP TimeFormatPicture
timeMarker = do
  _ <- count 2 t
  return TimeMarker

timeMarkers :: ReadP TimeFormatPicture
timeMarkers = timeMarker <++ timeMarkerShort

-- | Although not documented at
-- https://docs.microsoft.com/en-us/windows/win32/intl/hour--minute--and-second-format-pictures
-- the format pictures used by Windows do not require all such characters to be
-- enclosed in single quotation marks.
nonTimeSpecial :: ReadP Char
nonTimeSpecial = satisfy (\c -> c `notElem` ['h', 'H', 'm', 's', 't', '\''])

nonTimeSpecials :: ReadP String
nonTimeSpecials = greedy1 nonTimeSpecial

timeOther :: ReadP TimeFormatPicture
timeOther = do
  chars <- greedy1 (nonTimeSpecials <++ quotedChars)
  return $ TimeOther $ concat chars

timePicture :: ReadP [TimeFormatPicture]
timePicture = greedy (hours <++ minutes <++ seconds <++ timeMarkers <++
                     timeOther)

greedy :: ReadP a -> ReadP [a]
greedy p = greedy1 p <++ return []

greedy1 :: ReadP a -> ReadP [a]
greedy1 p = do
  first <- p
  rest <- greedy p
  return (first : rest)

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
  case readP_to_S parser input of
    [] -> Nothing
    ((result, _):_) -> Just result

-- | Translate from a Windows API day, month, year, and era format picture to
-- the closest corresponding format string used by
-- 'Data.Time.Format.formatTime'.
fromDateFormatPicture :: String -> Maybe String
fromDateFormatPicture dfp =
  fmap (concatMap fromDFP) $ parseMaybe datePicture dfp

-- | Translate from a Windows API hours, minute, and second format picture to
-- the closest corresponding format string used by
-- 'Data.Time.Format.formatTime'.
fromTimeFormatPicture :: String -> Maybe String
fromTimeFormatPicture tfp =
  fmap (concatMap fromTFP) $ parseMaybe timePicture tfp
