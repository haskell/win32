#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Win32.Icon
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

module Graphics.Win32.Icon where

import Foreign (Ptr)
import Graphics.Win32.GDI.Types
import System.Win32.Types

#include "windows_cconv.h"

----------------------------------------------------------------
-- Icons
----------------------------------------------------------------

createIcon :: HINSTANCE -> Int -> Int -> BYTE -> BYTE -> Ptr BYTE -> Ptr BYTE -> IO HICON
createIcon instance_ width height planes bitsPixel andBits xorBits =
    failIfNull "CreateIcon" $ c_CreateIcon instance_ width height planes bitsPixel andBits xorBits
foreign import WINDOWS_CCONV unsafe "windows.h CreateIcon"
    c_CreateIcon :: HINSTANCE -> Int -> Int -> BYTE -> BYTE -> Ptr BYTE -> Ptr BYTE -> IO HICON

copyIcon :: HICON -> IO HICON
copyIcon icon =
  failIfNull "CopyIcon" $ c_CopyIcon icon
foreign import WINDOWS_CCONV unsafe "windows.h CopyIcon"
  c_CopyIcon :: HICON -> IO HICON

drawIcon :: HDC -> Int -> Int -> HICON -> IO ()
drawIcon dc x y icon =
  failIfFalse_ "DrawIcon" $ c_DrawIcon dc x y icon
foreign import WINDOWS_CCONV unsafe "windows.h DrawIcon"
  c_DrawIcon :: HDC -> Int -> Int -> HICON -> IO Bool

destroyIcon :: HICON -> IO ()
destroyIcon icon =
  failIfFalse_ "DestroyIcon" $ c_DestroyIcon icon
foreign import WINDOWS_CCONV unsafe "windows.h DestroyIcon"
  c_DestroyIcon :: HICON -> IO Bool

----------------------------------------------------------------
-- End
----------------------------------------------------------------
