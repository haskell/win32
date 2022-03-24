#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Path.Internal
-- Copyright   :  (c) Tamar Christina, 1997-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Tamar Christina <tamar@zhox.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for interfacing with Win32.
--
-----------------------------------------------------------------------------

module System.Win32.Path.Internal where

import System.Win32.Types

##include "windows_cconv.h"

#include <windows.h>

foreign import WINDOWS_CCONV unsafe "Shlwapi.h PathRelativePathToW"
         c_pathRelativePathTo :: LPTSTR -> LPCTSTR -> DWORD -> LPCTSTR -> DWORD -> IO UINT
