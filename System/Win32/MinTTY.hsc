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
-- Much of this code was originally authored by Phil Ruffwind and the
-- git-for-windows project.
--
-----------------------------------------------------------------------------

module System.Win32.MinTTY (isMinTTY, isMinTTYHandle) where

import Graphics.Win32.Misc
import System.Win32.DLL
import System.Win32.File
import System.Win32.Types

#if MIN_VERSION_base(4,6,0)
import Control.Exception (catch)
#endif
import Control.Monad (void)
import Data.List (isInfixOf, isSuffixOf)
import Data.Word (Word8)
import Foreign hiding (void)
import Foreign.C.String
import Foreign.C.Types

#if __GLASGOW_HASKELL__ < 711
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

-- The headers that are shipped with GHC's copy of MinGW-w64 assume Windows XP.
-- Since we need some structs that are only available with Vista or later,
-- we must manually set WINVER/_WIN32_WINNT accordingly.
#define WINVER 0x0600
#define _WIN32_WINNT 0x0600
##include "windows_cconv.h"
#include <windows.h>

-- | Returns 'True' if the current process's standard error is attached to a
-- MinTTY console (e.g., Cygwin or MSYS). Returns 'False' otherwise.
isMinTTY :: IO Bool
isMinTTY = do
    h <- getStdHandle sTD_ERROR_HANDLE
           `catch` \(_ :: IOError) ->
             pure nullHANDLE
    if h == nullHANDLE
       then pure False
       else isMinTTYHandle h

-- | Returns 'True' is the given handle is attached to a MinTTY console
-- (e.g., Cygwin or MSYS). Returns 'False' otherwise.
isMinTTYHandle :: HANDLE -> IO Bool
isMinTTYHandle h = do
    fileType <- getFileType h
    if fileType /= fILE_TYPE_PIPE
      then pure False
      else isMinTTYVista h `catch` \(_ :: IOError) -> isMinTTYCompat h
      -- GetFileNameByHandleEx is only available on Vista and later (hence
      -- the name isMinTTYVista). If we're on an older version of Windows,
      -- getProcAddress will throw an IOException when it fails to find
      -- GetFileNameByHandleEx, and thus we will default to using
      -- NtQueryObject (isMinTTYCompat).

isMinTTYVista :: HANDLE -> IO Bool
isMinTTYVista h = do
    fn <- getFileNameByHandle h
    pure $ cygwinMSYSCheck fn
  `catch` \(_ :: IOError) ->
    pure False

isMinTTYCompat :: HANDLE -> IO Bool
isMinTTYCompat h = do
    fn <- ntQueryObjectNameInformation h
    pure $ cygwinMSYSCheck fn
  `catch` \(_ :: IOError) ->
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
    fni <- peek buf
    return $ fniFileName fni

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

data FILE_NAME_INFO = FILE_NAME_INFO
  { fniFileNameLength :: DWORD
  , fniFileName       :: String
  } deriving Show

instance Storable FILE_NAME_INFO where
    sizeOf    _ = #size      FILE_NAME_INFO
    alignment _ = #alignment FILE_NAME_INFO
    poke buf fni = do
        withCWStringLen (fniFileName fni) $ \(str, len) -> do
            let len'  = (min mAX_PATH len) * sizeOf (undefined :: CWchar)
                start = advancePtr (castPtr buf) (#offset FILE_NAME_INFO, FileName)
            (#poke FILE_NAME_INFO, FileNameLength) buf len'
            copyArray start (castPtr str :: Ptr Word8) len'
    peek buf = do
        vfniFileNameLength <- (#peek FILE_NAME_INFO, FileNameLength) buf
        let len = fromIntegral vfniFileNameLength `div` sizeOf (undefined :: CWchar)
        vfniFileName <- peekCWStringLen (plusPtr buf (#offset FILE_NAME_INFO, FileName), len)
        return $ FILE_NAME_INFO
          { fniFileNameLength = vfniFileNameLength
          , fniFileName       = vfniFileName
          }

type F_NtQueryObject a =
  HANDLE -> CInt -> Ptr a -> ULONG -> Ptr ULONG -> IO NTSTATUS

foreign import WINDOWS_CCONV "dynamic"
  mk_NtQueryObject :: FunPtr (F_NtQueryObject a) -> F_NtQueryObject a

type NTSTATUS = #type NTSTATUS
