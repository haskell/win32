-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Win32.Key
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

module Graphics.Win32.Key where

import Graphics.Win32.GDI.Types
import System.Win32.Types

import Control.Monad (liftM)

#include <windows.h>

type VKey   = DWORD

#{enum VKey,
 , vK_LBUTTON   = VK_LBUTTON
 , vK_RBUTTON   = VK_RBUTTON
 , vK_CANCEL    = VK_CANCEL
 , vK_MBUTTON   = VK_MBUTTON
 , vK_BACK      = VK_BACK
 , vK_TAB       = VK_TAB
 , vK_CLEAR     = VK_CLEAR
 , vK_RETURN    = VK_RETURN
 , vK_SHIFT     = VK_SHIFT
 , vK_CONTROL   = VK_CONTROL
 , vK_MENU      = VK_MENU
 , vK_PAUSE     = VK_PAUSE
 , vK_CAPITAL   = VK_CAPITAL
 , vK_ESCAPE    = VK_ESCAPE
 , vK_SPACE     = VK_SPACE
 , vK_PRIOR     = VK_PRIOR
 , vK_NEXT      = VK_NEXT
 , vK_END       = VK_END
 , vK_HOME      = VK_HOME
 , vK_LEFT      = VK_LEFT
 , vK_UP        = VK_UP
 , vK_RIGHT     = VK_RIGHT
 , vK_DOWN      = VK_DOWN
 , vK_SELECT    = VK_SELECT
 , vK_EXECUTE   = VK_EXECUTE
 , vK_SNAPSHOT  = VK_SNAPSHOT
 , vK_INSERT    = VK_INSERT
 , vK_DELETE    = VK_DELETE
 , vK_HELP      = VK_HELP
 , vK_NUMPAD0   = VK_NUMPAD0
 , vK_NUMPAD1   = VK_NUMPAD1
 , vK_NUMPAD2   = VK_NUMPAD2
 , vK_NUMPAD3   = VK_NUMPAD3
 , vK_NUMPAD4   = VK_NUMPAD4
 , vK_NUMPAD5   = VK_NUMPAD5
 , vK_NUMPAD6   = VK_NUMPAD6
 , vK_NUMPAD7   = VK_NUMPAD7
 , vK_NUMPAD8   = VK_NUMPAD8
 , vK_NUMPAD9   = VK_NUMPAD9
 , vK_MULTIPLY  = VK_MULTIPLY
 , vK_ADD       = VK_ADD
 , vK_SEPARATOR = VK_SEPARATOR
 , vK_SUBTRACT  = VK_SUBTRACT
 , vK_DECIMAL   = VK_DECIMAL
 , vK_DIVIDE    = VK_DIVIDE
 , vK_F1        = VK_F1
 , vK_F2        = VK_F2
 , vK_F3        = VK_F3
 , vK_F4        = VK_F4
 , vK_F5        = VK_F5
 , vK_F6        = VK_F6
 , vK_F7        = VK_F7
 , vK_F8        = VK_F8
 , vK_F9        = VK_F9
 , vK_F10       = VK_F10
 , vK_F11       = VK_F11
 , vK_F12       = VK_F12
 , vK_F13       = VK_F13
 , vK_F14       = VK_F14
 , vK_F15       = VK_F15
 , vK_F16       = VK_F16
 , vK_F17       = VK_F17
 , vK_F18       = VK_F18
 , vK_F19       = VK_F19
 , vK_F20       = VK_F20
 , vK_F21       = VK_F21
 , vK_F22       = VK_F22
 , vK_F23       = VK_F23
 , vK_F24       = VK_F24
 , vK_NUMLOCK   = VK_NUMLOCK
 , vK_SCROLL    = VK_SCROLL
 }

foreign import ccall unsafe "windows.h EnableWindow"
  enableWindow :: HWND -> Bool -> IO Bool

getActiveWindow :: IO (Maybe HWND)
getActiveWindow = liftM ptrToMaybe c_GetActiveWindow
foreign import ccall unsafe "windows.h GetActiveWindow"
  c_GetActiveWindow :: IO HWND

foreign import ccall unsafe "windows.h GetAsyncKeyState"
  getAsyncKeyState :: Int -> IO WORD

getFocus :: IO (Maybe HWND)
getFocus = liftM ptrToMaybe c_GetFocus
foreign import ccall unsafe "windows.h GetFocus"
  c_GetFocus :: IO HWND

foreign import ccall unsafe "windows.h GetKBCodePage"
  getKBCodePage :: IO UINT

foreign import ccall unsafe "windows.h IsWindowEnabled"
  isWindowEnabled :: HWND -> IO Bool
