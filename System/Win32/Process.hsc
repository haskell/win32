-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Process
-- Copyright   :  (c) Alastair Reid, 1997-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Esa Ilari vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for interfacing with Win32.
--
-----------------------------------------------------------------------------

module System.Win32.Process
  ( sleep       -- :: DWORD -> IO ()
  , iNFINITE    -- :: DWORD
  ) where

import System.Win32.Types

#include <windows.h>

-- constant to wait for a very long time.
iNFINITE :: DWORD
iNFINITE = #{const INFINITE}

foreign import stdcall unsafe "windows.h Sleep"
  sleep :: DWORD -> IO ()
