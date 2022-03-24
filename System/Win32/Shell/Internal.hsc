#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Shell.Internal
-- Copyright   :  (c) The University of Glasgow 2009
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- Win32 stuff from shell32.dll
--
-----------------------------------------------------------------------------

module System.Win32.Shell.Internal (
   c_SHGetFolderPath
 , raiseUnsupported
 ) where

import System.Win32.Types
import Graphics.Win32.GDI.Types (HWND)

import Foreign.C
import System.IO.Error

##include "windows_cconv.h"

-- for SHGetFolderPath stuff
#define _WIN32_IE 0x500
#include <windows.h>
#include <shlobj.h>

----------------------------------------------------------------
-- SHGetFolderPath
--
-- XXX: this is deprecated in Vista and later
----------------------------------------------------------------

raiseUnsupported :: String -> IO ()
raiseUnsupported loc =
   ioError (ioeSetErrorString (mkIOError illegalOperationErrorType loc Nothing Nothing) "unsupported operation")

foreign import WINDOWS_CCONV unsafe "SHGetFolderPathW"
  c_SHGetFolderPath :: HWND -> CInt -> HANDLE -> DWORD -> LPTSTR
                    -> IO HRESULT
