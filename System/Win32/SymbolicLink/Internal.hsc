{-# LANGUAGE CPP #-}
{- |
   Module      :  System.Win32.SymbolicLink.Internal
   Copyright   :  2012 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)
-}
module System.Win32.SymbolicLink.Internal where

import System.Win32.Types

##include "windows_cconv.h"

type SymbolicLinkFlags = DWORD

#{enum SymbolicLinkFlags,
 , sYMBOLIC_LINK_FLAG_FILE      = 0x0
 , sYMBOLIC_LINK_FLAG_DIRECTORY = 0x1
 , sYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE = 0x2
}

foreign import WINDOWS_CCONV unsafe "windows.h CreateSymbolicLinkW"
  c_CreateSymbolicLink :: LPTSTR -> LPTSTR -> SymbolicLinkFlags -> IO BOOL
