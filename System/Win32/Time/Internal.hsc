#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Time.Internal
-- Copyright   :  (c) Esa Ilari Vuokko, 2006
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for interfacing with Win32 Time API.
--
-----------------------------------------------------------------------------
module System.Win32.Time.Internal where

import System.Win32.Types   ( BOOL, DDWORD, DWORD, HANDLE, LARGE_INTEGER, LCID
                            , LONG, LPCTSTR, LPCWSTR, LPTSTR, LPWSTR, UINT, WORD
                            , dwordsToDdword, ddwordToDwords
                            )
import Control.Monad    ( when )
import Data.Word        ( Word8 )
import Foreign          ( Storable(sizeOf, alignment, peekByteOff, peek,
                                   pokeByteOff, poke)
                        , Ptr, castPtr, plusPtr, advancePtr
                        , copyArray )
import Foreign.C        ( CInt(..), CWchar(..)
                        , peekCWString, withCWStringLen )

##include "windows_cconv.h"
#include <windows.h>
#include "alignment.h"
#include "winnls_compat.h"

----------------------------------------------------------------
-- data types
----------------------------------------------------------------

newtype FILETIME = FILETIME DDWORD deriving (Show, Eq, Ord)

data SYSTEMTIME = SYSTEMTIME {
    wYear, wMonth, wDayOfWeek, wDay, wHour, wMinute, wSecond, wMilliseconds :: WORD }
    deriving (Show, Eq, Ord)

data TIME_ZONE_INFORMATION = TIME_ZONE_INFORMATION
    { tziBias :: LONG
    , tziStandardName :: String
    , tziStandardDate :: SYSTEMTIME
    , tziStandardBias :: LONG
    , tziDaylightName :: String
    , tziDaylightDate :: SYSTEMTIME
    , tziDaylightBias :: LONG
    } deriving (Show,Eq,Ord)

data TimeZoneId = TzIdUnknown | TzIdStandard | TzIdDaylight
    deriving (Show, Eq, Ord)

data LASTINPUTINFO = LASTINPUTINFO DWORD deriving (Show)

----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance Storable FILETIME where
    sizeOf = const (#size FILETIME)
    alignment _ = #alignment FILETIME
    poke buf (FILETIME n) = do
        (#poke FILETIME, dwLowDateTime) buf low
        (#poke FILETIME, dwHighDateTime) buf hi
        where (hi,low) = ddwordToDwords n
    peek buf = do
        low <- (#peek FILETIME, dwLowDateTime) buf
        hi <- (#peek FILETIME, dwHighDateTime) buf
        return $ FILETIME $ dwordsToDdword (hi,low)

instance Storable SYSTEMTIME where
    sizeOf _ = #size SYSTEMTIME
    alignment _ = #alignment SYSTEMTIME
    poke buf st = do
         (#poke SYSTEMTIME, wYear)          buf (wYear st)
         (#poke SYSTEMTIME, wMonth)         buf (wMonth st)
         (#poke SYSTEMTIME, wDayOfWeek)     buf (wDayOfWeek st)
         (#poke SYSTEMTIME, wDay)           buf (wDay st)
         (#poke SYSTEMTIME, wHour)          buf (wHour st)
         (#poke SYSTEMTIME, wMinute)        buf (wMinute st)
         (#poke SYSTEMTIME, wSecond)        buf (wSecond st)
         (#poke SYSTEMTIME, wMilliseconds)  buf (wMilliseconds st)
    peek buf = do
        year    <- (#peek SYSTEMTIME, wYear)        buf
        month   <- (#peek SYSTEMTIME, wMonth)       buf
        dow     <- (#peek SYSTEMTIME, wDayOfWeek)   buf
        day     <- (#peek SYSTEMTIME, wDay)         buf
        hour    <- (#peek SYSTEMTIME, wHour)        buf
        mins    <- (#peek SYSTEMTIME, wMinute)      buf
        sec     <- (#peek SYSTEMTIME, wSecond)      buf
        ms      <- (#peek SYSTEMTIME, wMilliseconds) buf
        return $ SYSTEMTIME year month dow day hour mins sec ms

instance Storable TIME_ZONE_INFORMATION where
    sizeOf _ = (#size TIME_ZONE_INFORMATION)
    alignment _ = #alignment TIME_ZONE_INFORMATION
    poke buf tzi = do
        (#poke TIME_ZONE_INFORMATION, Bias) buf (tziBias tzi)
        (#poke TIME_ZONE_INFORMATION, StandardDate) buf (tziStandardDate tzi)
        (#poke TIME_ZONE_INFORMATION, StandardBias) buf (tziStandardBias tzi)
        (#poke TIME_ZONE_INFORMATION, DaylightDate) buf (tziDaylightDate tzi)
        (#poke TIME_ZONE_INFORMATION, DaylightBias) buf (tziDaylightBias tzi)
        write buf (#offset TIME_ZONE_INFORMATION, StandardName) (tziStandardName tzi)
        write buf (#offset TIME_ZONE_INFORMATION, DaylightName) (tziDaylightName tzi)
        where
            write buf_ offset str = withCWStringLen str $ \(c_str,len) -> do
                when (len>31) $ fail "Storable TIME_ZONE_INFORMATION.poke: Too long string."
                let len'  = len * sizeOf (undefined :: CWchar)
                    start = (advancePtr (castPtr buf_) offset)
                    end   = advancePtr start len'
                copyArray start (castPtr c_str :: Ptr Word8) len'
                poke (castPtr end) (0 :: CWchar)

    peek buf = do
        bias <- (#peek TIME_ZONE_INFORMATION, Bias)         buf
        sdat <- (#peek TIME_ZONE_INFORMATION, StandardDate) buf
        sbia <- (#peek TIME_ZONE_INFORMATION, StandardBias) buf
        ddat <- (#peek TIME_ZONE_INFORMATION, DaylightDate) buf
        dbia <- (#peek TIME_ZONE_INFORMATION, DaylightBias) buf
        snam <- peekCWString (plusPtr buf (#offset TIME_ZONE_INFORMATION, StandardName))
        dnam <- peekCWString (plusPtr buf (#offset TIME_ZONE_INFORMATION, DaylightName))
        return $ TIME_ZONE_INFORMATION bias snam sdat sbia dnam ddat dbia

instance Storable LASTINPUTINFO where
    sizeOf = const (#size LASTINPUTINFO)
    alignment = sizeOf
    poke buf (LASTINPUTINFO t) = do
        (#poke LASTINPUTINFO, cbSize) buf ((#size LASTINPUTINFO) :: UINT)
        (#poke LASTINPUTINFO, dwTime) buf t
    peek buf = do
        t <- (#peek LASTINPUTINFO, dwTime) buf
        return $ LASTINPUTINFO t

foreign import WINDOWS_CCONV "windows.h GetSystemTime"
    c_GetSystemTime :: Ptr SYSTEMTIME -> IO ()

foreign import WINDOWS_CCONV "windows.h SetSystemTime"
    c_SetSystemTime :: Ptr SYSTEMTIME -> IO BOOL

foreign import WINDOWS_CCONV "windows.h GetSystemTimeAsFileTime"
    c_GetSystemTimeAsFileTime :: Ptr FILETIME -> IO ()

foreign import WINDOWS_CCONV "windows.h GetLocalTime"
    c_GetLocalTime :: Ptr SYSTEMTIME -> IO ()

foreign import WINDOWS_CCONV "windows.h SetLocalTime"
    c_SetLocalTime :: Ptr SYSTEMTIME -> IO BOOL

foreign import WINDOWS_CCONV "windows.h GetSystemTimeAdjustment"
    c_GetSystemTimeAdjustment :: Ptr DWORD -> Ptr DWORD -> Ptr BOOL -> IO BOOL

foreign import WINDOWS_CCONV "windows.h GetTickCount" getTickCount :: IO DWORD

foreign import WINDOWS_CCONV unsafe "windows.h GetLastInputInfo"
  c_GetLastInputInfo :: Ptr LASTINPUTINFO -> IO Bool

foreign import WINDOWS_CCONV "windows.h SetSystemTimeAdjustment"
    c_SetSystemTimeAdjustment :: DWORD -> BOOL -> IO BOOL

foreign import WINDOWS_CCONV "windows.h GetTimeZoneInformation"
    c_GetTimeZoneInformation :: Ptr TIME_ZONE_INFORMATION -> IO DWORD

foreign import WINDOWS_CCONV "windows.h SystemTimeToFileTime"
    c_SystemTimeToFileTime :: Ptr SYSTEMTIME -> Ptr FILETIME -> IO BOOL

foreign import WINDOWS_CCONV "windows.h FileTimeToSystemTime"
    c_FileTimeToSystemTime :: Ptr FILETIME -> Ptr SYSTEMTIME -> IO BOOL

foreign import WINDOWS_CCONV "windows.h GetFileTime"
    c_GetFileTime :: HANDLE -> Ptr FILETIME -> Ptr FILETIME -> Ptr FILETIME -> IO BOOL

foreign import WINDOWS_CCONV "windows.h SetFileTime"
    c_SetFileTime :: HANDLE -> Ptr FILETIME -> Ptr FILETIME -> Ptr FILETIME -> IO BOOL

foreign import WINDOWS_CCONV "windows.h FileTimeToLocalFileTime"
    c_FileTimeToLocalFileTime :: Ptr FILETIME -> Ptr FILETIME -> IO BOOL

foreign import WINDOWS_CCONV "windows.h LocalFileTimeToFileTime"
    c_LocalFileTimeToFileTime :: Ptr FILETIME -> Ptr FILETIME -> IO BOOL

{-
-- Windows XP SP1
foreign import WINDOWS_CCONV "windows.h GetSystemTimes"
    c_GetSystemTimes :: Ptr FILETIME -> Ptr FILETIME -> Ptr FILETIME -> IO BOOL
getSystemTimes :: IO (FILETIME,FILETIME,FILETIME)
getSystemTimes = alloca $ \idle -> alloca $ \kernel -> alloca $ \user -> do
    failIf not "getSystemTimes: GetSystemTimes" $ c_GetSystemTimes idle kernel user
    liftM3 (,,) (peek idle) (peek kernel) (peek user)
-}

{-
-- Windows XP
foreign import WINDOWS_CCONV "windows.h SystemTimeToTzSpecificLocalTime"
    c_SystemTimeToTzSpecificLocalTime :: Ptr TIME_ZONE_INFORMATION -> Ptr SYSTEMTIME -> Ptr SYSTEMTIME -> IO BOOL
systemTimeToTzSpecificLocalTime :: TIME_ZONE_INFORMATION -> SYSTEMTIME -> IO SYSTEMTIME
systemTimeToTzSpecificLocalTime tzi st = with tzi $ \tzi -> with st $ \st -> alloca $ \res -> do
    failIf not "systemTimeToTzSpecificLocalTime: SystemTimeToTzSpecificLocalTime" $
        c_SystemTimeToTzSpecificLocalTime tzi st res
    peek res

foreign import WINDOWS_CCONV "windows.h TzSpecificLocalTimeToSystemTime"
    c_TzSpecificLocalTimeToSystemTime :: Ptr TIME_ZONE_INFORMATION -> Ptr SYSTEMTIME -> Ptr SYSTEMTIME -> IO BOOL
tzSpecificLocalTimeToSystemTime :: TIME_ZONE_INFORMATION -> SYSTEMTIME -> IO SYSTEMTIME
tzSpecificLocalTimeToSystemTime tzi st = with tzi $ \tzi -> with st $ \st -> alloca $ \res -> do
    failIf not "tzSpecificLocalTimeToSystemTime: TzSpecificLocalTimeToSystemTime" $
        c_TzSpecificLocalTimeToSystemTime tzi st res
    peek res
-}

foreign import WINDOWS_CCONV "windows.h QueryPerformanceFrequency"
    c_QueryPerformanceFrequency :: Ptr LARGE_INTEGER -> IO BOOL

foreign import WINDOWS_CCONV "windows.h QueryPerformanceCounter"
    c_QueryPerformanceCounter:: Ptr LARGE_INTEGER -> IO BOOL

type GetTimeFormatFlags = DWORD
#{enum GetTimeFormatFlags,
    , lOCALE_NOUSEROVERRIDE = LOCALE_NOUSEROVERRIDE
    , lOCALE_USE_CP_ACP     = LOCALE_USE_CP_ACP
    , tIME_NOMINUTESORSECONDS = TIME_NOMINUTESORSECONDS
    , tIME_NOSECONDS        = TIME_NOSECONDS
    , tIME_NOTIMEMARKER     = TIME_NOTIMEMARKER
    , tIME_FORCE24HOURFORMAT= TIME_FORCE24HOURFORMAT
    }

foreign import WINDOWS_CCONV "windows.h GetTimeFormatEx"
    c_GetTimeFormatEx :: LPCWSTR
                      -> GetTimeFormatFlags
                      -> Ptr SYSTEMTIME
                      -> LPCWSTR
                      -> LPWSTR
                      -> CInt
                      -> IO CInt

foreign import WINDOWS_CCONV "windows.h GetTimeFormatW"
    c_GetTimeFormat :: LCID -> GetTimeFormatFlags -> Ptr SYSTEMTIME -> LPCTSTR -> LPTSTR -> CInt -> IO CInt
