#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Info
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

module System.Win32.Info
    ( SystemColor
    , cOLOR_SCROLLBAR
    , cOLOR_BACKGROUND
    , cOLOR_ACTIVECAPTION
    , cOLOR_INACTIVECAPTION
    , cOLOR_MENU
    , cOLOR_WINDOW
    , cOLOR_WINDOWFRAME
    , cOLOR_MENUTEXT
    , cOLOR_WINDOWTEXT
    , cOLOR_CAPTIONTEXT
    , cOLOR_ACTIVEBORDER
    , cOLOR_INACTIVEBORDER
    , cOLOR_APPWORKSPACE
    , cOLOR_HIGHLIGHT
    , cOLOR_HIGHLIGHTTEXT
    , cOLOR_BTNFACE
    , cOLOR_BTNSHADOW
    , cOLOR_GRAYTEXT
    , cOLOR_BTNTEXT
    , cOLOR_INACTIVECAPTIONTEXT
    , cOLOR_BTNHIGHLIGHT

      -- * Standard directories
    , getSystemDirectory
    , getWindowsDirectory
    , getCurrentDirectory
    , getTemporaryDirectory
    , getFullPathName
    , getLongPathName
    , getShortPathName
    , searchPath

      -- * System information
    , ProcessorArchitecture(..)
    , SYSTEM_INFO(..)
    , getSystemInfo

      -- * System metrics
    , SMSetting
    , sM_ARRANGE
    , sM_CLEANBOOT
    , sM_CMETRICS
    , sM_CMOUSEBUTTONS
    , sM_CXBORDER
    , sM_CYBORDER
    , sM_CXCURSOR
    , sM_CYCURSOR
    , sM_CXDLGFRAME
    , sM_CYDLGFRAME
    , sM_CXDOUBLECLK
    , sM_CYDOUBLECLK
    , sM_CXDRAG
    , sM_CYDRAG
    , sM_CXEDGE
    , sM_CYEDGE
    , sM_CXFRAME
    , sM_CYFRAME
    , sM_CXFULLSCREEN
    , sM_CYFULLSCREEN
    , sM_CXHSCROLL
    , sM_CYVSCROLL
    , sM_CXICON
    , sM_CYICON
    , sM_CXICONSPACING
    , sM_CYICONSPACING
    , sM_CXMAXIMIZED
    , sM_CYMAXIMIZED
    , sM_CXMENUCHECK
    , sM_CYMENUCHECK
    , sM_CXMENUSIZE
    , sM_CYMENUSIZE
    , sM_CXMIN
    , sM_CYMIN
    , sM_CXMINIMIZED
    , sM_CYMINIMIZED
    , sM_CXMINTRACK
    , sM_CYMINTRACK
    , sM_CXSCREEN
    , sM_CYSCREEN
    , sM_CXSIZE
    , sM_CYSIZE
    , sM_CXSIZEFRAME
    , sM_CYSIZEFRAME
    , sM_CXSMICON
    , sM_CYSMICON
    , sM_CXSMSIZE
    , sM_CYSMSIZE
    , sM_CXVSCROLL
    , sM_CYHSCROLL
    , sM_CYVTHUMB
    , sM_CYCAPTION
    , sM_CYKANJIWINDOW
    , sM_CYMENU
    , sM_CYSMCAPTION
    , sM_DBCSENABLED
    , sM_DEBUG
    , sM_MENUDROPALIGNMENT
    , sM_MIDEASTENABLED
    , sM_MOUSEPRESENT
    , sM_NETWORK
    , sM_PENWINDOWS
    , sM_SECURE
    , sM_SHOWSOUNDS
    , sM_SLOWMACHINE
    , sM_SWAPBUTTON

      -- * User name
    , getUserName
    ) where

import System.Win32.Info.Internal
import Control.Exception (catch)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with, maybeWith)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (Storable(..))
import System.IO.Error (isDoesNotExistError)
import System.Win32.Types (failIfFalse_, peekTStringLen, withTString, try)

#if !MIN_VERSION_base(4,6,0)
import Prelude hiding (catch)
#endif

##include "windows_cconv.h"

#include <windows.h>
#include "alignment.h"

----------------------------------------------------------------
-- Environment Strings
----------------------------------------------------------------

-- %fun ExpandEnvironmentStrings :: String -> IO String

----------------------------------------------------------------
-- Computer Name
----------------------------------------------------------------

-- %fun GetComputerName :: IO String
-- %fun SetComputerName :: String -> IO ()
-- %end free(arg1)

----------------------------------------------------------------
-- Hardware Profiles
----------------------------------------------------------------

-- %fun GetCurrentHwProfile :: IO HW_PROFILE_INFO

----------------------------------------------------------------
-- Keyboard Type
----------------------------------------------------------------

-- %fun GetKeyboardType :: KeyboardTypeKind -> IO KeyboardType

----------------------------------------------------------------
-- Standard Directories
----------------------------------------------------------------

getSystemDirectory :: IO String
getSystemDirectory = try "GetSystemDirectory" c_getSystemDirectory 512

getWindowsDirectory :: IO String
getWindowsDirectory = try "GetWindowsDirectory" c_getWindowsDirectory 512

getCurrentDirectory :: IO String
getCurrentDirectory = try "GetCurrentDirectory" (flip c_getCurrentDirectory) 512

getTemporaryDirectory :: IO String
getTemporaryDirectory = try "GetTempPath" (flip c_getTempPath) 512

getFullPathName :: FilePath -> IO FilePath
getFullPathName name = do
  withTString name $ \ c_name ->
    try "getFullPathName"
      (\buf len -> c_GetFullPathName c_name len buf nullPtr) 512

getLongPathName :: FilePath -> IO FilePath
getLongPathName name = do
  withTString name $ \ c_name ->
    try "getLongPathName"
      (c_GetLongPathName c_name) 512

getShortPathName :: FilePath -> IO FilePath
getShortPathName name = do
  withTString name $ \ c_name ->
    try "getShortPathName"
      (c_GetShortPathName c_name) 512

searchPath :: Maybe String -> FilePath -> Maybe String -> IO (Maybe FilePath)
searchPath path filename ext =
  maybe ($ nullPtr) withTString path $ \p_path ->
  withTString filename $ \p_filename ->
  maybeWith withTString ext      $ \p_ext ->
  alloca $ \ppFilePart -> (do
    s <- try "searchPath" (\buf len -> c_SearchPath p_path p_filename p_ext
                          len buf ppFilePart) 512
    return (Just s))
     `catch` \e -> if isDoesNotExistError e
                       then return Nothing
                       else ioError e

----------------------------------------------------------------
-- System Info (Info about processor and memory subsystem)
----------------------------------------------------------------

getSystemInfo :: IO SYSTEM_INFO
getSystemInfo = alloca $ \ret -> do
    c_GetSystemInfo ret
    peek ret

-- %fun GetSystemMetrics :: SMSetting -> IO Int

----------------------------------------------------------------
-- Thread Desktops
----------------------------------------------------------------

-- %fun GetThreadDesktop :: ThreadId -> IO HDESK
-- %fun SetThreadDesktop :: ThreadId -> HDESK -> IO ()

----------------------------------------------------------------
-- User name
----------------------------------------------------------------

-- %fun GetUserName :: IO String

getUserName :: IO String
getUserName =
  allocaArray 512 $ \ c_str ->
    with 512 $ \ c_len -> do
        failIfFalse_ "GetUserName" $ c_GetUserName c_str c_len
        len <- peek c_len
        peekTStringLen (c_str, fromIntegral len - 1)

----------------------------------------------------------------
-- End
----------------------------------------------------------------
