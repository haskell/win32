{-# LANGUAGE ScopedTypeVariables #-}

#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.MinTTY
-- Copyright   :  (c) University of Glasgow 2006
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- A function to check if the current terminal uses MinTTY.
-- Much of this code was originally authored by Phil Ruffwind.
--
-----------------------------------------------------------------------------

module System.Win32.MinTTY (isMinTTY) where

import Graphics.Win32.Misc
import System.Win32.DLL
import System.Win32.File
import System.Win32.Types

#if MIN_VERSION_base(4,6,0)
import Control.Exception (catch)
#endif
import Control.Monad (void)
import Data.List (isInfixOf, isSuffixOf)
import Foreign hiding (void)
import Foreign.C.String
import Foreign.C.Types

#if __GLASGOW_HASKELL__ < 711
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

##include "windows_cconv.h"
#include <windows.h>

-- | Returns 'True' is running on a MinTTY console (e.g., Cygwin or MSYS).
-- Returns 'False' otherwise.
isMinTTY :: IO Bool
isMinTTY = do
    h <- getStdHandle sTD_ERROR_HANDLE
    fileType <- getFileType h
    if fileType /= fILE_TYPE_PIPE
      then pure False
      else do
        -- GetFileNameByHandleEx is only available on Vista and later, so if
        -- we're on an older version of Windows, we must default to using
        -- NtQueryObject.
        vistaPlus <- isVistaOrLater
        if vistaPlus
           then isMinTTYVista h
           else isMinTTYCompat h

isMinTTYVista :: HANDLE -> IO Bool
isMinTTYVista h = do
    fn <- getFileNameByHandle h
    pure $ cygwinMSYSCheck fn
  `catch` \ (_ :: IOError) ->
    pure False

isMinTTYCompat :: HANDLE -> IO Bool
isMinTTYCompat h = do
    fn <- ntQueryObjectNameInformation h
    pure $ cygwinMSYSCheck fn
  `catch` \ (_ :: IOError) ->
    pure False

cygwinMSYSCheck :: String -> Bool
cygwinMSYSCheck fn = ("\\cygwin-" `isInfixOf` fn || "\\msys-" `isInfixOf` fn) &&
            "-pty" `isInfixOf` fn &&
            "-master" `isSuffixOf` fn
-- Note that GetFileInformationByHandleEx might return something like:
--
--    \msys-dd50a72ab4668b33-pty1-to-master
--
-- But NtQueryObject might return something like:
--
--    \Device\NamedPipe\msys-dd50a72ab4668b33-pty1-to-master
--
-- This means we can't rely on "cygwin-" or "msys-" being at the very start,
-- so we check for their presence using `isInfixOf` instead of `isPrefixOf`.

getFileNameByHandle :: HANDLE -> IO String
getFileNameByHandle h = do
  let sizeOfDWORD = sizeOf (undefined :: DWORD)
  let sizeOfWchar = sizeOf (undefined :: CWchar)
  -- note: implicitly assuming that DWORD has stronger alignment than wchar_t
  let bufSize = sizeOfDWORD + mAX_PATH * sizeOfWchar
  allocaBytes bufSize $ \buf -> do
    getFileInformationByHandleEx h fILE_NAME_INFO buf (fromIntegral bufSize)
    len :: DWORD <- peek buf
    let len' = fromIntegral len `div` sizeOfWchar
    peekCWStringLen (buf `plusPtr` sizeOfDWORD, min len' mAX_PATH)

getFileInformationByHandleEx
  :: HANDLE -> CInt -> Ptr a -> DWORD -> IO ()
getFileInformationByHandleEx h cls buf bufSize = do
  lib <- getModuleHandle (Just "kernel32.dll")
  ptr <- getProcAddress lib "GetFileInformationByHandleEx"
  let c_GetFileInformationByHandleEx =
        mk_GetFileInformationByHandleEx (castPtrToFunPtr ptr)
  failIfFalse_ "getFileInformationByHandleEx"
    (c_GetFileInformationByHandleEx h cls buf bufSize)

ntQueryObjectNameInformation :: HANDLE -> IO String
ntQueryObjectNameInformation h = do
  let sizeOfUSHORT = sizeOf (undefined :: USHORT)
  let sizeOfWchar  = sizeOf (undefined :: CWchar)
  let bufSize = 8 * sizeOfUSHORT + mAX_PATH * sizeOfWchar
  allocaBytes bufSize $ \buf ->
    alloca $ \p_len -> do
      ntQueryObject h oBJECT_NAME_INFORMATION buf (fromIntegral bufSize) p_len
      len :: USHORT <- peek buf
      let len' = fromIntegral len `div` sizeOfWchar
      peekCWStringLen (buf `plusPtr` (8 * sizeOfUSHORT), min len' mAX_PATH)

ntQueryObject :: HANDLE -> CInt -> Ptr a -> ULONG -> Ptr ULONG -> IO ()
ntQueryObject h cls buf bufSize p_len = do
  lib <- getModuleHandle (Just "ntdll.dll")
  ptr <- getProcAddress lib "NtQueryObject"
  let c_NtQueryObject = mk_NtQueryObject (castPtrToFunPtr ptr)
  void $ failIfNeg "NtQueryObject" $ c_NtQueryObject h cls buf bufSize p_len

isVistaOrLater :: IO Bool
isVistaOrLater = do
  lib <- getModuleHandle (Just "ntdll.dll")
  ptr <- getProcAddress lib "RtlGetVersion"
  let c_RtlGetVersion = mk_RtlGetVersion (castPtrToFunPtr ptr)
  osVersionInfo <- alloca $ \p -> do
    _ <- failIfNeg "RtlGetVersion" $ c_RtlGetVersion p
    peek p
  return $ dwMajorVersion osVersionInfo >= 6

fILE_NAME_INFO :: CInt
fILE_NAME_INFO = 2

mAX_PATH :: Num a => a
mAX_PATH = #const MAX_PATH

oBJECT_NAME_INFORMATION :: CInt
oBJECT_NAME_INFORMATION = 1

type F_GetFileInformationByHandleEx a =
  HANDLE -> CInt -> Ptr a -> DWORD -> IO BOOL

foreign import WINDOWS_CCONV "dynamic"
  mk_GetFileInformationByHandleEx
  :: FunPtr (F_GetFileInformationByHandleEx a)
  -> F_GetFileInformationByHandleEx a

type F_NtQueryObject a =
  HANDLE -> CInt -> Ptr a -> ULONG -> Ptr ULONG -> IO NTSTATUS

foreign import WINDOWS_CCONV "dynamic"
  mk_NtQueryObject :: FunPtr (F_NtQueryObject a) -> F_NtQueryObject a

type F_RtlGetVersion = Ptr RTL_OSVERSIONINFOW -> IO NTSTATUS

foreign import WINDOWS_CCONV "dynamic"
  mk_RtlGetVersion :: FunPtr F_RtlGetVersion -> F_RtlGetVersion

type ULONG    = #type ULONG
type NTSTATUS = #type NTSTATUS

data RTL_OSVERSIONINFOW = RTL_OSVERSIONINFOW
  { dwOSVersionInfoSize :: ULONG
  , dwMajorVersion      :: ULONG
  , dwMinorVersion      :: ULONG
  , dwBuildNumber       :: ULONG
  , dwPlatformId        :: ULONG
  , szCSDVersion        :: CWString
  } deriving Show

instance Storable RTL_OSVERSIONINFOW where
  sizeOf    _ = #size      RTL_OSVERSIONINFOW
  alignment _ = #alignment RTL_OSVERSIONINFOW
  poke buf ovi = do
      (#poke RTL_OSVERSIONINFOW, dwOSVersionInfoSize) buf (dwOSVersionInfoSize ovi)
      (#poke RTL_OSVERSIONINFOW, dwMajorVersion)      buf (dwMajorVersion      ovi)
      (#poke RTL_OSVERSIONINFOW, dwMinorVersion)      buf (dwMinorVersion      ovi)
      (#poke RTL_OSVERSIONINFOW, dwBuildNumber)       buf (dwBuildNumber       ovi)
      (#poke RTL_OSVERSIONINFOW, dwPlatformId)        buf (dwPlatformId        ovi)
      (#poke RTL_OSVERSIONINFOW, szCSDVersion)        buf (dwPlatformId        ovi)
  peek buf = do
      osVersionInfoSize <- (#peek RTL_OSVERSIONINFOW, dwOSVersionInfoSize) buf
      majorVersion      <- (#peek RTL_OSVERSIONINFOW, dwMajorVersion)      buf
      minorVersion      <- (#peek RTL_OSVERSIONINFOW, dwMinorVersion)      buf
      buildNumber       <- (#peek RTL_OSVERSIONINFOW, dwBuildNumber)       buf
      platformId        <- (#peek RTL_OSVERSIONINFOW, dwPlatformId)        buf
      csdVersion        <- (#peek RTL_OSVERSIONINFOW, szCSDVersion)        buf
      return $ RTL_OSVERSIONINFOW
        { dwOSVersionInfoSize = osVersionInfoSize
        , dwMajorVersion      = majorVersion
        , dwMinorVersion      = minorVersion
        , dwBuildNumber       = buildNumber
        , dwPlatformId        = platformId
        , szCSDVersion        = csdVersion
        }
