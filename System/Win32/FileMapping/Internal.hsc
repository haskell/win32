#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.FileMapping.Internal
-- Copyright   :  (c) Esa Ilari Vuokko, 2006
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for interfacing with Win32 mapped files.
--
-----------------------------------------------------------------------------
module System.Win32.FileMapping.Internal where

import System.Win32.Types   ( HANDLE, DWORD, BOOL, SIZE_T, LPCTSTR )

import Foreign                  ( Ptr, FunPtr )
import Foreign.C.Types (CUIntPtr(..))

##include "windows_cconv.h"

#include "windows.h"

---------------------------------------------------------------------------
-- Derived functions
---------------------------------------------------------------------------

data MappedObject = MappedObject HANDLE HANDLE FileMapAccess


---------------------------------------------------------------------------
-- Enums
---------------------------------------------------------------------------
type ProtectSectionFlags = DWORD
#{enum ProtectSectionFlags,
    , sEC_COMMIT    = SEC_COMMIT
    , sEC_IMAGE     = SEC_IMAGE
    , sEC_NOCACHE   = SEC_NOCACHE
    , sEC_RESERVE   = SEC_RESERVE
    }
type FileMapAccess = DWORD
#{enum FileMapAccess,
    , fILE_MAP_ALL_ACCESS   = FILE_MAP_ALL_ACCESS
    , fILE_MAP_COPY         = FILE_MAP_COPY
    , fILE_MAP_READ         = FILE_MAP_READ
    , fILE_MAP_WRITE        = FILE_MAP_WRITE
    }

---------------------------------------------------------------------------
-- Imports
---------------------------------------------------------------------------
foreign import WINDOWS_CCONV "windows.h OpenFileMappingW"
    c_OpenFileMapping :: DWORD -> BOOL -> LPCTSTR -> IO HANDLE

foreign import WINDOWS_CCONV "windows.h CreateFileMappingW"
    c_CreateFileMapping :: HANDLE -> Ptr () -> DWORD -> DWORD -> DWORD -> LPCTSTR -> IO HANDLE

foreign import WINDOWS_CCONV "windows.h MapViewOfFileEx"
    c_MapViewOfFileEx :: HANDLE -> DWORD -> DWORD -> DWORD -> SIZE_T -> Ptr a -> IO (Ptr b)

foreign import WINDOWS_CCONV "windows.h UnmapViewOfFile"
    c_UnmapViewOfFile :: Ptr a -> IO BOOL

{-# CFILES cbits/HsWin32.c #-}
foreign import ccall "HsWin32.h &UnmapViewOfFileFinaliser"
    c_UnmapViewOfFileFinaliser :: FunPtr (Ptr a -> IO ())
