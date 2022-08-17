-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.DLL
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

module System.Win32.WindowsString.DLL
    ( module System.Win32.WindowsString.DLL
    , module System.Win32.DLL
    ) where

import System.Win32.DLL hiding
  (   disableThreadLibraryCalls
    , freeLibrary
    , getModuleFileName
    , getModuleHandle
    , getProcAddress
    , loadLibrary
    , loadLibraryEx
    , setDllDirectory
    , lOAD_LIBRARY_AS_DATAFILE
    , lOAD_WITH_ALTERED_SEARCH_PATH
  )
import System.Win32.DLL.Internal
import System.Win32.WindowsString.Types

import Foreign
import Data.Maybe (fromMaybe)
import System.OsString.Windows
import GHC.IO.Encoding.UTF16 ( mkUTF16le )
import GHC.IO.Encoding.Failure ( CodingFailureMode(..) )

getModuleFileName :: HMODULE -> IO WindowsString
getModuleFileName hmod =
  allocaArray 512 $ \ c_str -> do
  failIfFalse_ "GetModuleFileName" $ c_GetModuleFileName hmod c_str 512
  peekTString c_str

getModuleHandle :: Maybe WindowsString -> IO HMODULE
getModuleHandle mb_name =
  maybeWith withTString mb_name $ \ c_name ->
  failIfNull "GetModuleHandle" $ c_GetModuleHandle c_name

loadLibrary :: WindowsString -> IO HMODULE
loadLibrary name =
  withTString name $ \ c_name ->
  failIfNull "LoadLibrary" $ c_LoadLibrary c_name

loadLibraryEx :: WindowsString -> HANDLE -> LoadLibraryFlags -> IO HMODULE
loadLibraryEx name h flags =
  withTString name $ \ c_name ->
  failIfNull "LoadLibraryEx" $ c_LoadLibraryEx c_name h flags

setDllDirectory :: Maybe WindowsString -> IO ()
setDllDirectory name =
  maybeWith withTString name $ \ c_name -> do
    let nameS = name >>= either (const Nothing) Just . decodeWith (mkUTF16le TransliterateCodingFailure)
    failIfFalse_ (unwords ["SetDllDirectory", fromMaybe "NULL" nameS]) $ c_SetDllDirectory c_name

