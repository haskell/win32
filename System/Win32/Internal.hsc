#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Internal
-- Copyright   :  (c) Alastair Reid, 1997-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Bryan O'Sullivan <bos@serpentine.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for interfacing with Win32.
--
-----------------------------------------------------------------------------

module System.Win32.Internal
        ( module System.Win32.Internal
        ) where

import System.Win32.Types

##include "windows_cconv.h"

#include <windows.h>

getCurrentCodePage :: IO CodePage
getCurrentCodePage = do
    conCP <- getConsoleCP
    if conCP > 0
        then return conCP
        else getACP

-- ToDo: various enum functions.

#{enum CodePage,
 , cP_ACP       = CP_ACP
 , cP_MACCP     = CP_MACCP
 , cP_OEMCP     = CP_OEMCP
 }

foreign import WINDOWS_CCONV unsafe "windows.h GetACP"
  getACP :: IO CodePage

foreign import WINDOWS_CCONV unsafe "windows.h GetConsoleCP"
  getConsoleCP :: IO UINT

foreign import WINDOWS_CCONV unsafe "windows.h SetConsoleCP"
   setConsoleCP :: UINT -> IO ()

foreign import WINDOWS_CCONV unsafe "windows.h GetConsoleOutputCP"
   getConsoleOutputCP :: IO UINT

foreign import WINDOWS_CCONV unsafe "windows.h SetConsoleOutputCP"
   setConsoleOutputCP :: UINT -> IO ()

