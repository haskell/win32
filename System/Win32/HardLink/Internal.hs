{-# LANGUAGE CPP #-}
{- |
   Module      :  System.Win32.HardLink.Internal
   Copyright   :  2013 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Handling hard link using Win32 API. [NTFS only]

   Note: You should worry about file system type when use this module's function in your application:

     * NTFS only supprts this functionality.

     * ReFS doesn't support hard link currently.
-}
module System.Win32.HardLink.Internal where

import System.Win32.File   ( LPSECURITY_ATTRIBUTES )
import System.Win32.String ( LPCTSTR )
import System.Win32.Types  ( BOOL )

#include "windows_cconv.h"

foreign import WINDOWS_CCONV unsafe "windows.h CreateHardLinkW"
  c_CreateHardLink :: LPCTSTR -- ^ Hard link name
                   -> LPCTSTR -- ^ Target file path
                   -> LPSECURITY_ATTRIBUTES -- ^ This parameter is reserved. You should pass just /nullPtr/.
                   -> IO BOOL
