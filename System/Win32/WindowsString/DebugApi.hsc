-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.WindowsString.DebugApi
-- Copyright   :  (c) Esa Ilari Vuokko, 2006
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for using Windows DebugApi.
--
-----------------------------------------------------------------------------
module System.Win32.WindowsString.DebugApi
    ( module System.Win32.WindowsString.DebugApi
    , module System.Win32.DebugApi
    ) where

import System.Win32.DebugApi.Internal
import System.Win32.DebugApi hiding (outputDebugString)
import System.Win32.WindowsString.Types   ( withTString )
import System.OsString.Windows

##include "windows_cconv.h"
#include "windows.h"


--------------------------------------------------------------------------
-- On process being debugged

outputDebugString :: WindowsString -> IO ()
outputDebugString s = withTString s $ \c_s -> c_OutputDebugString c_s

