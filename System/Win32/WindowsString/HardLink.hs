{-# LANGUAGE CPP #-}
{- |
   Module      :  System.Win32.HardLink
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
module System.Win32.WindowsString.HardLink
  ( createHardLink
  , createHardLink'
  ) where

import System.Win32.HardLink.Internal
import System.Win32.WindowsString.File   ( failIfFalseWithRetry_ )
import System.Win32.WindowsString.String ( withTString )
import System.Win32.WindowsString.Types  ( nullPtr )
import System.OsPath.Windows

#include "windows_cconv.h"

-- | NOTE: createHardLink is /flipped arguments/ to provide compatibility for Unix.
-- 
-- If you want to create hard link by Windows way, use 'createHardLink'' instead.
createHardLink :: WindowsPath -- ^ Target file path
               -> WindowsPath -- ^ Hard link name
               -> IO ()
createHardLink = flip createHardLink'

createHardLink' :: WindowsPath -- ^ Hard link name
                -> WindowsPath -- ^ Target file path
                -> IO ()
createHardLink' link target =
   withTString target $ \c_target ->
   withTString link   $ \c_link ->
        failIfFalseWithRetry_ (unwords ["CreateHardLinkW",show link,show target]) $
          c_CreateHardLink c_link c_target nullPtr
