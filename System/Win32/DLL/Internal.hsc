#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.DLL.Internal
-- Copyright   :  (c) Alastair Reid, 1997-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for interfacing with Win32.
--
-----------------------------------------------------------------------------

module System.Win32.DLL.Internal where

import System.Win32.Types

##include "windows_cconv.h"

#include <windows.h>

foreign import WINDOWS_CCONV unsafe "windows.h DisableThreadLibraryCalls"
  c_DisableThreadLibraryCalls :: HMODULE -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h FreeLibrary"
  c_FreeLibrary :: HMODULE -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h GetModuleFileNameW"
  c_GetModuleFileName :: HMODULE -> LPTSTR -> Int -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h GetModuleHandleW"
  c_GetModuleHandle :: LPCTSTR -> IO HMODULE

foreign import WINDOWS_CCONV unsafe "windows.h GetProcAddress"
  c_GetProcAddress :: HMODULE -> LPCSTR -> IO Addr

foreign import WINDOWS_CCONV unsafe "windows.h LoadLibraryW"
  c_LoadLibrary :: LPCTSTR -> IO HINSTANCE

type LoadLibraryFlags = DWORD

#{enum LoadLibraryFlags,
 , lOAD_LIBRARY_AS_DATAFILE      = LOAD_LIBRARY_AS_DATAFILE
 , lOAD_WITH_ALTERED_SEARCH_PATH = LOAD_WITH_ALTERED_SEARCH_PATH
 }

foreign import WINDOWS_CCONV unsafe "windows.h LoadLibraryExW"
  c_LoadLibraryEx :: LPCTSTR -> HANDLE -> LoadLibraryFlags -> IO HINSTANCE

foreign import WINDOWS_CCONV unsafe "windows.h SetDllDirectoryW"
  c_SetDllDirectory :: LPTSTR -> IO BOOL
