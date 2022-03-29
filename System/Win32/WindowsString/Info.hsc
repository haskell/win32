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

module System.Win32.WindowsString.Info
    ( module System.Win32.WindowsString.Info
    , module System.Win32.Info
    ) where

import System.Win32.Info.Internal
import System.Win32.Info hiding (
    getSystemDirectory
  , getWindowsDirectory
  , getCurrentDirectory
  , getTemporaryDirectory
  , getFullPathName
  , getLongPathName
  , getShortPathName
  , searchPath
  , getUserName
  )
import Control.Exception (catch)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with, maybeWith)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (Storable(..))
import System.IO.Error (isDoesNotExistError)
import System.Win32.WindowsString.Types (failIfFalse_, peekTStringLen, withTString, try)
import System.OsPath.Windows

#if !MIN_VERSION_base(4,6,0)
import Prelude hiding (catch)
#endif

##include "windows_cconv.h"

#include <windows.h>
#include "alignment.h"

----------------------------------------------------------------
-- Standard Directories
----------------------------------------------------------------

getSystemDirectory :: IO WindowsString
getSystemDirectory = try "GetSystemDirectory" c_getSystemDirectory 512

getWindowsDirectory :: IO WindowsString
getWindowsDirectory = try "GetWindowsDirectory" c_getWindowsDirectory 512

getCurrentDirectory :: IO WindowsString
getCurrentDirectory = try "GetCurrentDirectory" (flip c_getCurrentDirectory) 512

getTemporaryDirectory :: IO WindowsString
getTemporaryDirectory = try "GetTempPath" (flip c_getTempPath) 512

getFullPathName :: WindowsPath -> IO WindowsPath
getFullPathName name = do
  withTString name $ \ c_name ->
    try "getFullPathName"
      (\buf len -> c_GetFullPathName c_name len buf nullPtr) 512

getLongPathName :: WindowsPath -> IO WindowsPath
getLongPathName name = do
  withTString name $ \ c_name ->
    try "getLongPathName"
      (c_GetLongPathName c_name) 512

getShortPathName :: WindowsPath -> IO WindowsPath
getShortPathName name = do
  withTString name $ \ c_name ->
    try "getShortPathName"
      (c_GetShortPathName c_name) 512

searchPath :: Maybe WindowsString -> WindowsPath -> Maybe WindowsString -> IO (Maybe WindowsPath)
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
-- User name
----------------------------------------------------------------

-- %fun GetUserName :: IO String

getUserName :: IO WindowsString
getUserName =
  allocaArray 512 $ \ c_str ->
    with 512 $ \ c_len -> do
        failIfFalse_ "GetUserName" $ c_GetUserName c_str c_len
        len <- peek c_len
        peekTStringLen (c_str, fromIntegral len - 1)

----------------------------------------------------------------
-- End
----------------------------------------------------------------
