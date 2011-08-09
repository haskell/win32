#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Win32.GDI.Clip
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

module Graphics.Win32.GDI.Clip where

import Graphics.Win32.GDI.Types
import System.Win32.Types

import Foreign

#include <windows.h>

type ClipboardFormat = UINT

#{enum ClipboardFormat,
 , cF_BITMAP            = CF_BITMAP
 , cF_DIB               = CF_DIB
 , cF_DIF               = CF_DIF
 , cF_DSPBITMAP         = CF_DSPBITMAP
 , cF_DSPENHMETAFILE    = CF_DSPENHMETAFILE
 , cF_DSPMETAFILEPICT   = CF_DSPMETAFILEPICT
 , cF_DSPTEXT           = CF_DSPTEXT
 , cF_ENHMETAFILE       = CF_ENHMETAFILE
 , cF_GDIOBJFIRST       = CF_GDIOBJFIRST
 , cF_HDROP             = CF_HDROP
 , cF_LOCALE            = CF_LOCALE
 , cF_METAFILEPICT      = CF_METAFILEPICT
 , cF_OEMTEXT           = CF_OEMTEXT
 , cF_OWNERDISPLAY      = CF_OWNERDISPLAY
 , cF_PALETTE           = CF_PALETTE
 , cF_PENDATA           = CF_PENDATA
 , cF_PRIVATEFIRST      = CF_PRIVATEFIRST
 , cF_PRIVATELAST       = CF_PRIVATELAST
 , cF_RIFF              = CF_RIFF
 , cF_SYLK              = CF_SYLK
 , cF_TEXT              = CF_TEXT
 , cF_WAVE              = CF_WAVE
 , cF_TIFF              = CF_TIFF
 }

-- % , CF_UNICODETEXT  -- WinNT only

foreign import stdcall unsafe "windows.h ChangeClipboardChain"
  changeClipboardChain :: HWND -> HWND -> IO Bool

closeClipboard :: IO ()
closeClipboard =
  failIfFalse_ "CloseClipboard" c_CloseClipboard
foreign import stdcall unsafe "windows.h CloseClipboard"
  c_CloseClipboard :: IO BOOL

foreign import stdcall unsafe "windows.h CountClipboardFormats"
  countClipboardFormats :: IO Int

emptyClipboard :: IO ()
emptyClipboard =
  failIfFalse_ "EmptyClipboard" c_EmptyClipboard
foreign import stdcall unsafe "windows.h EmptyClipboard"
  c_EmptyClipboard :: IO BOOL

-- original also tested GetLastError() != NO_ERROR

enumClipboardFormats :: ClipboardFormat -> IO ClipboardFormat
enumClipboardFormats format =
  failIfZero "EnumClipboardFormats" $ c_EnumClipboardFormats format
foreign import stdcall unsafe "windows.h EnumClipboardFormats"
  c_EnumClipboardFormats :: ClipboardFormat -> IO ClipboardFormat

getClipboardData :: ClipboardFormat -> IO HANDLE
getClipboardData format =
  failIfNull "GetClipboardData" $ c_GetClipboardData format
foreign import stdcall unsafe "windows.h GetClipboardData"
  c_GetClipboardData :: ClipboardFormat -> IO HANDLE

getClipboardFormatName :: ClipboardFormat -> IO String
getClipboardFormatName format =
  allocaArray 256 $ \ c_name -> do
  len <- failIfZero "GetClipboardFormatName" $
    c_GetClipboardFormatName format c_name 256
  peekTStringLen (c_name, len)
foreign import stdcall unsafe "windows.h GetClipboardFormatNameW"
  c_GetClipboardFormatName :: ClipboardFormat -> LPTSTR -> Int -> IO Int

getClipboardOwner :: IO HWND
getClipboardOwner =
  failIfNull "GetClipboardOwner" c_GetClipboardOwner
foreign import stdcall unsafe "windows.h GetClipboardOwner"
  c_GetClipboardOwner :: IO HWND

getClipboardViewer :: IO HWND
getClipboardViewer =
  failIfNull "GetClipboardViewer" c_GetClipboardViewer
foreign import stdcall unsafe "windows.h GetClipboardViewer"
  c_GetClipboardViewer :: IO HWND

getOpenClipboardWindow :: IO HWND
getOpenClipboardWindow =
  failIfNull "GetClipboardWindow" c_GetOpenClipboardWindow
foreign import stdcall unsafe "windows.h GetOpenClipboardWindow"
  c_GetOpenClipboardWindow :: IO HWND

getPriorityClipboardFormat :: [ClipboardFormat] -> IO Int
getPriorityClipboardFormat formats =
  withArray formats $ \ format_array ->
  failIf (== -1) "GetPriorityClipboardFormat" $
    c_GetPriorityClipboardFormat format_array (length formats)
foreign import stdcall unsafe "windows.h GetPriorityClipboardFormat"
  c_GetPriorityClipboardFormat :: Ptr UINT -> Int -> IO Int

foreign import stdcall unsafe "windows.h IsClipboardFormatAvailable"
  isClipboardFormatAvailable :: ClipboardFormat -> IO BOOL

openClipboard :: HWND -> IO ()
openClipboard wnd =
  failIfFalse_ "OpenClipboard" $ c_OpenClipboard wnd
foreign import stdcall unsafe "windows.h OpenClipboard"
  c_OpenClipboard :: HWND -> IO BOOL

registerClipboardFormat :: String -> IO ClipboardFormat
registerClipboardFormat name =
  withTString name $ \ c_name ->
  failIfZero "RegisterClipboardFormat" $
    c_RegisterClipboardFormat c_name
foreign import stdcall unsafe "windows.h RegisterClipboardFormatW"
  c_RegisterClipboardFormat :: LPCTSTR -> IO ClipboardFormat

setClipboardData :: ClipboardFormat -> HANDLE -> IO HANDLE
setClipboardData format mem =
  failIfNull "SetClipboardData" $ c_SetClipboardData format mem
foreign import stdcall unsafe "windows.h SetClipboardData"
  c_SetClipboardData :: ClipboardFormat -> HANDLE -> IO HANDLE

setClipboardViewer :: HWND -> IO HWND
setClipboardViewer wnd =
  failIfNull "SetClipboardViewer" $ c_SetClipboardViewer wnd
foreign import stdcall unsafe "windows.h SetClipboardViewer"
  c_SetClipboardViewer :: HWND -> IO HWND
