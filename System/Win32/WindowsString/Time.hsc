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
module System.Win32.WindowsString.Time
    ( module System.Win32.WindowsString.Time
    , module System.Win32.Time
    ) where

import System.Win32.Time.Internal
import System.Win32.Time hiding (getTimeFormatEx, getTimeFormat)

import System.Win32.WindowsString.String  ( peekTStringLen, withTString )
import System.Win32.WindowsString.Types   ( LCID, failIf )
import System.Win32.Utils   ( trySized )

import Foreign          ( Storable(sizeOf)
                        , nullPtr, castPtr
                        , with, allocaBytes )
import Foreign.C        ( CWchar(..)
                        , withCWString )
import Foreign.Marshal.Utils (maybeWith)
import System.OsString.Windows

##include "windows_cconv.h"
#include <windows.h>
#include "alignment.h"
#include "winnls_compat.h"


getTimeFormatEx :: Maybe WindowsString
                -> GetTimeFormatFlags
                -> Maybe SYSTEMTIME
                -> Maybe WindowsString
                -> IO String
getTimeFormatEx locale flags st fmt =
    maybeWith withTString locale $ \c_locale ->
        maybeWith with st $ \c_st ->
            maybeWith withTString fmt $ \c_fmt -> do
                let c_func = c_GetTimeFormatEx c_locale flags c_st c_fmt
                trySized "GetTimeFormatEx" c_func

getTimeFormat :: LCID -> GetTimeFormatFlags -> Maybe SYSTEMTIME -> Maybe String -> IO WindowsString
getTimeFormat locale flags st fmt =
    maybeWith with st $ \c_st ->
    maybeWith withCWString fmt $ \c_fmt -> do
        size <- c_GetTimeFormat locale flags c_st c_fmt nullPtr 0
        allocaBytes ((fromIntegral size) * (sizeOf (undefined::CWchar))) $ \out -> do
            size' <- failIf (==0) "getTimeFormat: GetTimeFormat" $
                c_GetTimeFormat locale flags c_st c_fmt (castPtr out) size
            peekTStringLen (out,fromIntegral size')
