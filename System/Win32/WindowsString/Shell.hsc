{-# LANGUAGE Trustworthy #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.WindowsString.Shell
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

module System.Win32.WindowsString.Shell (
  sHGetFolderPath,
  CSIDL,
  cSIDL_PROFILE,
  cSIDL_APPDATA,
  cSIDL_WINDOWS,
  cSIDL_PERSONAL,
  cSIDL_LOCAL_APPDATA,
  cSIDL_DESKTOPDIRECTORY,
  cSIDL_PROGRAM_FILES,
  SHGetFolderPathFlags,
  sHGFP_TYPE_CURRENT,
  sHGFP_TYPE_DEFAULT
 ) where

import System.OsString.Windows (WindowsString)
import System.Win32.Shell.Internal
import System.Win32.Shell hiding (sHGetFolderPath)
import System.Win32.WindowsString.Types
import Graphics.Win32.GDI.Types (HWND)

import Foreign
import Control.Monad

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


sHGetFolderPath :: HWND -> CSIDL -> HANDLE -> SHGetFolderPathFlags -> IO WindowsString
sHGetFolderPath hwnd csidl hdl flags =
  allocaBytes ((#const MAX_PATH) * (#size TCHAR)) $ \pstr -> do
    r <- c_SHGetFolderPath hwnd csidl hdl flags pstr
    when (r < 0) $ raiseUnsupported "sHGetFolderPath"
    peekTString pstr
