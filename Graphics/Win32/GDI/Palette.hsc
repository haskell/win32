-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Win32.GDI.Palette
-- Copyright   :  (c) Alastair Reid, 1997-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for interfacing with Win32.
--
-----------------------------------------------------------------------------

module Graphics.Win32.GDI.Palette where

import System.Win32.Types
import Graphics.Win32.GDI.Types

#include <windows.h>

----------------------------------------------------------------
-- Palettes
----------------------------------------------------------------

type StockPalette   = WORD

#{enum StockPalette,
 , dEFAULT_PALETTE = DEFAULT_PALETTE
 }

getStockPalette :: StockPalette -> IO HPALETTE
getStockPalette sp =
  failIfNull "GetStockPalette" $ c_GetStockPalette sp
foreign import ccall unsafe "windows.h GetStockObject"
  c_GetStockPalette :: StockPalette -> IO HPALETTE

deletePalette :: HPALETTE -> IO ()
deletePalette p =
  failIfFalse_ "DeletePalette" $ c_DeletePalette p
foreign import ccall unsafe "windows.h DeleteObject"
  c_DeletePalette :: HPALETTE -> IO Bool

-- macros

foreign import ccall unsafe "HsWin32.h"
  pALETTERGB :: BYTE -> BYTE -> BYTE -> COLORREF

foreign import ccall unsafe "HsWin32.h"
  pALETTEINDEX :: WORD -> COLORREF

----------------------------------------------------------------
-- End
----------------------------------------------------------------
