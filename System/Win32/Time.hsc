#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Time
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
module System.Win32.Time
    ( FILETIME(..)
    , SYSTEMTIME(..)
    , TIME_ZONE_INFORMATION(..)
    , TimeZoneId(..)
    , getSystemTime
    , setSystemTime
    , getSystemTimeAsFileTime
    , getLocalTime
    , setLocalTime
    , getSystemTimeAdjustment
    , getTickCount
    , getLastInputInfo
    , getIdleTime
    , setSystemTimeAdjustment
    , getTimeZoneInformation
    , systemTimeToFileTime
    , fileTimeToSystemTime
    , getFileTime
    , setFileTime
    , invalidFileTime
    , fileTimeToLocalFileTime
    , localFileTimeToFileTime
    , queryPerformanceFrequency
    , queryPerformanceCounter
    , GetTimeFormatFlags
    , lOCALE_NOUSEROVERRIDE
    , lOCALE_USE_CP_ACP
    , tIME_NOMINUTESORSECONDS
    , tIME_NOSECONDS
    , tIME_NOTIMEMARKER
    , tIME_FORCE24HOURFORMAT
    , getTimeFormatEx
    , getTimeFormat
    ) where

import System.Win32.Time.Internal
import System.Win32.String  ( peekTStringLen, withTString )
import System.Win32.Types   ( DWORD, HANDLE, LCID
                            , failIf
                            , failIfFalse_, failIf_ )
import System.Win32.Utils   ( trySized )

import Control.Monad    ( liftM3, liftM )
import Foreign          ( Storable(sizeOf, peek)
                        , Ptr, nullPtr, castPtr
                        , with, alloca, allocaBytes )
import Foreign.C        ( CWchar(..)
                        , withCWString )
import Foreign.Marshal.Utils (maybeWith)

##include "windows_cconv.h"
#include <windows.h>
#include "alignment.h"
#include "winnls_compat.h"


getSystemTime :: IO SYSTEMTIME
getSystemTime = alloca $ \res -> do
    c_GetSystemTime res
    peek res

setSystemTime :: SYSTEMTIME -> IO ()
setSystemTime st = with st $ \c_st -> failIf_ not "setSystemTime: SetSystemTime" $
    c_SetSystemTime c_st

getSystemTimeAsFileTime :: IO FILETIME
getSystemTimeAsFileTime = alloca $ \ret -> do
    c_GetSystemTimeAsFileTime ret
    peek ret

getLocalTime :: IO SYSTEMTIME
getLocalTime = alloca $ \res -> do
    c_GetLocalTime res
    peek res

setLocalTime :: SYSTEMTIME -> IO ()
setLocalTime st = with st $ \c_st -> failIf_ not "setLocalTime: SetLocalTime" $
    c_SetLocalTime c_st

getSystemTimeAdjustment :: IO (Maybe (Int, Int))
getSystemTimeAdjustment = alloca $ \ta -> alloca $ \ti -> alloca $ \enabled -> do
    failIf_ not "getSystemTimeAdjustment: GetSystemTimeAdjustment" $
        c_GetSystemTimeAdjustment ta ti enabled
    enabled' <- peek enabled
    if enabled'
        then do
            ta' <- peek ta
            ti' <- peek ti
            return $ Just (fromIntegral ta', fromIntegral ti')
        else return Nothing

getLastInputInfo :: IO DWORD
getLastInputInfo =
  with (LASTINPUTINFO 0) $ \lii_p -> do
  failIfFalse_ "GetLastInputInfo" $ c_GetLastInputInfo lii_p
  LASTINPUTINFO lii <- peek lii_p
  return lii

getIdleTime :: IO Integer
getIdleTime = do
  lii <- getLastInputInfo
  now <- getTickCount
  return $ fromIntegral $ now - lii

setSystemTimeAdjustment :: Maybe Int -> IO ()
setSystemTimeAdjustment ta =
    failIf_ not "setSystemTimeAjustment: SetSystemTimeAdjustment" $
        c_SetSystemTimeAdjustment time disabled
    where
        (time,disabled) = case ta of
            Nothing -> (0,True)
            Just x  -> (fromIntegral x,False)

getTimeZoneInformation :: IO (TimeZoneId, TIME_ZONE_INFORMATION)
getTimeZoneInformation = alloca $ \tzi -> do
    tz <- failIf (==(#const TIME_ZONE_ID_INVALID)) "getTimeZoneInformation: GetTimeZoneInformation" $
        c_GetTimeZoneInformation tzi
    tzi' <- peek tzi
    return . flip (,) tzi' $ case tz of
        (#const TIME_ZONE_ID_UNKNOWN)   -> TzIdUnknown
        (#const TIME_ZONE_ID_STANDARD)  -> TzIdStandard
        (#const TIME_ZONE_ID_DAYLIGHT)  -> TzIdDaylight
        _                               -> TzIdUnknown   -- to remove warning

systemTimeToFileTime :: SYSTEMTIME -> IO FILETIME
systemTimeToFileTime s = with s $ \c_s -> alloca $ \ret -> do
    failIf_ not "systemTimeToFileTime: SystemTimeToFileTime" $
        c_SystemTimeToFileTime c_s ret
    peek ret

fileTimeToSystemTime :: FILETIME -> IO SYSTEMTIME
fileTimeToSystemTime s = with s $ \c_s -> alloca $ \ret -> do
    failIf_ not "fileTimeToSystemTime: FileTimeToSystemTime" $
        c_FileTimeToSystemTime c_s ret
    peek ret

getFileTime :: HANDLE -> IO (FILETIME,FILETIME,FILETIME)
getFileTime h = alloca $ \crt -> alloca $ \acc -> alloca $ \wrt -> do
    failIf_ not "getFileTime: GetFileTime" $ c_GetFileTime h crt acc wrt
    liftM3 (,,) (peek crt) (peek acc) (peek wrt)

invalidFileTime :: FILETIME
invalidFileTime = FILETIME 0

setFileTime :: HANDLE -> Maybe FILETIME -> Maybe FILETIME -> Maybe FILETIME -> IO ()
setFileTime h crt acc wrt = withTime crt $
    \c_crt -> withTime acc $
    \c_acc -> withTime wrt $
    \c_wrt -> do
      failIf_ not "setFileTime: SetFileTime" $ c_SetFileTime h c_crt c_acc c_wrt
  where
    withTime :: Maybe FILETIME -> (Ptr FILETIME -> IO a) -> IO a
    withTime Nothing k  = k nullPtr
    withTime (Just t) k = with t k

fileTimeToLocalFileTime :: FILETIME -> IO FILETIME
fileTimeToLocalFileTime ft = with ft $ \c_ft -> alloca $ \res -> do
    failIf_ not "fileTimeToLocalFileTime: FileTimeToLocalFileTime"
        $ c_FileTimeToLocalFileTime c_ft res
    peek res

localFileTimeToFileTime :: FILETIME -> IO FILETIME
localFileTimeToFileTime ft = with ft $ \c_ft -> alloca $ \res -> do
    failIf_ not "localFileTimeToFileTime: LocalFileTimeToFileTime"
        $ c_LocalFileTimeToFileTime c_ft res
    peek res

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

queryPerformanceFrequency :: IO Integer
queryPerformanceFrequency = alloca $ \res -> do
    failIf_ not "queryPerformanceFrequency: QueryPerformanceFrequency" $
        c_QueryPerformanceFrequency res
    liftM fromIntegral $ peek res

queryPerformanceCounter:: IO Integer
queryPerformanceCounter= alloca $ \res -> do
    failIf_ not "queryPerformanceCounter: QueryPerformanceCounter" $
        c_QueryPerformanceCounter res
    liftM fromIntegral $ peek res


getTimeFormatEx :: Maybe String
                -> GetTimeFormatFlags
                -> Maybe SYSTEMTIME
                -> Maybe String
                -> IO String
getTimeFormatEx locale flags st fmt =
    maybeWith withTString locale $ \c_locale ->
        maybeWith with st $ \c_st ->
            maybeWith withTString fmt $ \c_fmt -> do
                let c_func = c_GetTimeFormatEx c_locale flags c_st c_fmt
                trySized "GetTimeFormatEx" c_func

getTimeFormat :: LCID -> GetTimeFormatFlags -> Maybe SYSTEMTIME -> Maybe String -> IO String
getTimeFormat locale flags st fmt =
    maybeWith with st $ \c_st ->
    maybeWith withCWString fmt $ \c_fmt -> do
        size <- c_GetTimeFormat locale flags c_st c_fmt nullPtr 0
        allocaBytes ((fromIntegral size) * (sizeOf (undefined::CWchar))) $ \out -> do
            size' <- failIf (==0) "getTimeFormat: GetTimeFormat" $
                c_GetTimeFormat locale flags c_st c_fmt (castPtr out) size
            peekTStringLen (out,fromIntegral size')
