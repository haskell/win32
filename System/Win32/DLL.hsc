#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
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

module System.Win32.DLL
    ( disableThreadLibraryCalls
    , freeLibrary
    , getModuleFileName
    , getModuleHandle
    , getProcAddress
    , loadLibrary
    , loadLibraryEx
    , setDllDirectory
    , LoadLibraryFlags
    , lOAD_LIBRARY_AS_DATAFILE
    , lOAD_WITH_ALTERED_SEARCH_PATH
    ) where

import System.Win32.DLL.Internal
import System.Win32.Types

import Foreign
import Foreign.C
import Data.Maybe (fromMaybe)

disableThreadLibraryCalls :: HMODULE -> IO ()
disableThreadLibraryCalls hmod =
  failIfFalse_ "DisableThreadLibraryCalls" $ c_DisableThreadLibraryCalls hmod

freeLibrary :: HMODULE -> IO ()
freeLibrary hmod =
  failIfFalse_ "FreeLibrary" $ c_FreeLibrary hmod

getModuleFileName :: HMODULE -> IO String
getModuleFileName hmod =
  allocaArray 512 $ \ c_str -> do
  failIfFalse_ "GetModuleFileName" $ c_GetModuleFileName hmod c_str 512
  peekTString c_str

getModuleHandle :: Maybe String -> IO HMODULE
getModuleHandle mb_name =
  maybeWith withTString mb_name $ \ c_name ->
  failIfNull "GetModuleHandle" $ c_GetModuleHandle c_name

getProcAddress :: HMODULE -> String -> IO Addr
getProcAddress hmod procname =
  withCAString procname $ \ c_procname ->
  failIfNull "GetProcAddress" $ c_GetProcAddress hmod c_procname

loadLibrary :: String -> IO HINSTANCE
loadLibrary name =
  withTString name $ \ c_name ->
  failIfNull "LoadLibrary" $ c_LoadLibrary c_name

loadLibraryEx :: String -> HANDLE -> LoadLibraryFlags -> IO HINSTANCE
loadLibraryEx name h flags =
  withTString name $ \ c_name ->
  failIfNull "LoadLibraryEx" $ c_LoadLibraryEx c_name h flags

setDllDirectory :: Maybe String -> IO ()
setDllDirectory name =
  maybeWith withTString name $ \ c_name ->
  failIfFalse_ (unwords ["SetDllDirectory", fromMaybe "NULL" name]) $ c_SetDllDirectory c_name

