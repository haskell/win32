#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.File.Internal
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

module System.Win32.File.Internal where

import System.Win32.Types
import System.Win32.Time

import Foreign hiding (void)

##include "windows_cconv.h"

#include <windows.h>
#include "alignment.h"

----------------------------------------------------------------
-- Enumeration types
----------------------------------------------------------------

type AccessMode   = UINT

gENERIC_NONE :: AccessMode
gENERIC_NONE = 0

#{enum AccessMode,
 , gENERIC_READ              = GENERIC_READ
 , gENERIC_WRITE             = GENERIC_WRITE
 , gENERIC_EXECUTE           = GENERIC_EXECUTE
 , gENERIC_ALL               = GENERIC_ALL
 , dELETE                    = DELETE
 , rEAD_CONTROL              = READ_CONTROL
 , wRITE_DAC                 = WRITE_DAC
 , wRITE_OWNER               = WRITE_OWNER
 , sYNCHRONIZE               = SYNCHRONIZE
 , sTANDARD_RIGHTS_REQUIRED  = STANDARD_RIGHTS_REQUIRED
 , sTANDARD_RIGHTS_READ      = STANDARD_RIGHTS_READ
 , sTANDARD_RIGHTS_WRITE     = STANDARD_RIGHTS_WRITE
 , sTANDARD_RIGHTS_EXECUTE   = STANDARD_RIGHTS_EXECUTE
 , sTANDARD_RIGHTS_ALL       = STANDARD_RIGHTS_ALL
 , sPECIFIC_RIGHTS_ALL       = SPECIFIC_RIGHTS_ALL
 , aCCESS_SYSTEM_SECURITY    = ACCESS_SYSTEM_SECURITY
 , mAXIMUM_ALLOWED           = MAXIMUM_ALLOWED
 , fILE_ADD_FILE             = FILE_ADD_FILE
 , fILE_ADD_SUBDIRECTORY     = FILE_ADD_SUBDIRECTORY
 , fILE_ALL_ACCESS           = FILE_ALL_ACCESS
 , fILE_APPEND_DATA          = FILE_APPEND_DATA
 , fILE_CREATE_PIPE_INSTANCE = FILE_CREATE_PIPE_INSTANCE
 , fILE_DELETE_CHILD         = FILE_DELETE_CHILD
 , fILE_EXECUTE              = FILE_EXECUTE
 , fILE_LIST_DIRECTORY       = FILE_LIST_DIRECTORY
 , fILE_READ_ATTRIBUTES      = FILE_READ_ATTRIBUTES
 , fILE_READ_DATA            = FILE_READ_DATA
 , fILE_READ_EA              = FILE_READ_EA
 , fILE_TRAVERSE             = FILE_TRAVERSE
 , fILE_WRITE_ATTRIBUTES     = FILE_WRITE_ATTRIBUTES
 , fILE_WRITE_DATA           = FILE_WRITE_DATA
 , fILE_WRITE_EA             = FILE_WRITE_EA
 }

----------------------------------------------------------------

type ShareMode   = UINT

fILE_SHARE_NONE :: ShareMode
fILE_SHARE_NONE = 0

#{enum ShareMode,
 , fILE_SHARE_READ      = FILE_SHARE_READ
 , fILE_SHARE_WRITE     = FILE_SHARE_WRITE
 , fILE_SHARE_DELETE    = FILE_SHARE_DELETE
 }

----------------------------------------------------------------

type CreateMode   = UINT

#{enum CreateMode,
 , cREATE_NEW           = CREATE_NEW
 , cREATE_ALWAYS        = CREATE_ALWAYS
 , oPEN_EXISTING        = OPEN_EXISTING
 , oPEN_ALWAYS          = OPEN_ALWAYS
 , tRUNCATE_EXISTING    = TRUNCATE_EXISTING
 }

----------------------------------------------------------------

type FileAttributeOrFlag   = UINT

#{enum FileAttributeOrFlag,
 , fILE_ATTRIBUTE_READONLY      = FILE_ATTRIBUTE_READONLY
 , fILE_ATTRIBUTE_HIDDEN        = FILE_ATTRIBUTE_HIDDEN
 , fILE_ATTRIBUTE_SYSTEM        = FILE_ATTRIBUTE_SYSTEM
 , fILE_ATTRIBUTE_DIRECTORY     = FILE_ATTRIBUTE_DIRECTORY
 , fILE_ATTRIBUTE_ARCHIVE       = FILE_ATTRIBUTE_ARCHIVE
 , fILE_ATTRIBUTE_NORMAL        = FILE_ATTRIBUTE_NORMAL
 , fILE_ATTRIBUTE_TEMPORARY     = FILE_ATTRIBUTE_TEMPORARY
 , fILE_ATTRIBUTE_COMPRESSED    = FILE_ATTRIBUTE_COMPRESSED
 , fILE_ATTRIBUTE_REPARSE_POINT = FILE_ATTRIBUTE_REPARSE_POINT
 , fILE_FLAG_WRITE_THROUGH      = FILE_FLAG_WRITE_THROUGH
 , fILE_FLAG_OVERLAPPED         = FILE_FLAG_OVERLAPPED
 , fILE_FLAG_NO_BUFFERING       = FILE_FLAG_NO_BUFFERING
 , fILE_FLAG_RANDOM_ACCESS      = FILE_FLAG_RANDOM_ACCESS
 , fILE_FLAG_SEQUENTIAL_SCAN    = FILE_FLAG_SEQUENTIAL_SCAN
 , fILE_FLAG_DELETE_ON_CLOSE    = FILE_FLAG_DELETE_ON_CLOSE
 , fILE_FLAG_BACKUP_SEMANTICS   = FILE_FLAG_BACKUP_SEMANTICS
 , fILE_FLAG_POSIX_SEMANTICS    = FILE_FLAG_POSIX_SEMANTICS
 }
#ifndef __WINE_WINDOWS_H
#{enum FileAttributeOrFlag,
 , sECURITY_ANONYMOUS           = SECURITY_ANONYMOUS
 , sECURITY_IDENTIFICATION      = SECURITY_IDENTIFICATION
 , sECURITY_IMPERSONATION       = SECURITY_IMPERSONATION
 , sECURITY_DELEGATION          = SECURITY_DELEGATION
 , sECURITY_CONTEXT_TRACKING    = SECURITY_CONTEXT_TRACKING
 , sECURITY_EFFECTIVE_ONLY      = SECURITY_EFFECTIVE_ONLY
 , sECURITY_SQOS_PRESENT        = SECURITY_SQOS_PRESENT
 , sECURITY_VALID_SQOS_FLAGS    = SECURITY_VALID_SQOS_FLAGS
 }
#endif

----------------------------------------------------------------

type MoveFileFlag   = DWORD

#{enum MoveFileFlag,
 , mOVEFILE_REPLACE_EXISTING    = MOVEFILE_REPLACE_EXISTING
 , mOVEFILE_COPY_ALLOWED        = MOVEFILE_COPY_ALLOWED
 , mOVEFILE_DELAY_UNTIL_REBOOT  = MOVEFILE_DELAY_UNTIL_REBOOT
 }

----------------------------------------------------------------

type FilePtrDirection   = DWORD

#{enum FilePtrDirection,
 , fILE_BEGIN   = FILE_BEGIN
 , fILE_CURRENT = FILE_CURRENT
 , fILE_END     = FILE_END
 }

----------------------------------------------------------------

type DriveType = UINT

#{enum DriveType,
 , dRIVE_UNKNOWN        = DRIVE_UNKNOWN
 , dRIVE_NO_ROOT_DIR    = DRIVE_NO_ROOT_DIR
 , dRIVE_REMOVABLE      = DRIVE_REMOVABLE
 , dRIVE_FIXED          = DRIVE_FIXED
 , dRIVE_REMOTE         = DRIVE_REMOTE
 , dRIVE_CDROM          = DRIVE_CDROM
 , dRIVE_RAMDISK        = DRIVE_RAMDISK
 }

----------------------------------------------------------------

type DefineDosDeviceFlags = DWORD

#{enum DefineDosDeviceFlags,
 , dDD_RAW_TARGET_PATH          = DDD_RAW_TARGET_PATH
 , dDD_REMOVE_DEFINITION        = DDD_REMOVE_DEFINITION
 , dDD_EXACT_MATCH_ON_REMOVE    = DDD_EXACT_MATCH_ON_REMOVE
 }

----------------------------------------------------------------

type BinaryType = DWORD

#{enum BinaryType,
 , sCS_32BIT_BINARY     = SCS_32BIT_BINARY
 , sCS_DOS_BINARY       = SCS_DOS_BINARY
 , sCS_WOW_BINARY       = SCS_WOW_BINARY
 , sCS_PIF_BINARY       = SCS_PIF_BINARY
 , sCS_POSIX_BINARY     = SCS_POSIX_BINARY
 , sCS_OS216_BINARY     = SCS_OS216_BINARY
 }

----------------------------------------------------------------

type FileNotificationFlag = DWORD

#{enum FileNotificationFlag,
 , fILE_NOTIFY_CHANGE_FILE_NAME  = FILE_NOTIFY_CHANGE_FILE_NAME
 , fILE_NOTIFY_CHANGE_DIR_NAME   = FILE_NOTIFY_CHANGE_DIR_NAME
 , fILE_NOTIFY_CHANGE_ATTRIBUTES = FILE_NOTIFY_CHANGE_ATTRIBUTES
 , fILE_NOTIFY_CHANGE_SIZE       = FILE_NOTIFY_CHANGE_SIZE
 , fILE_NOTIFY_CHANGE_LAST_WRITE = FILE_NOTIFY_CHANGE_LAST_WRITE
 , fILE_NOTIFY_CHANGE_SECURITY   = FILE_NOTIFY_CHANGE_SECURITY
 }

----------------------------------------------------------------

type FileType = DWORD

#{enum FileType,
 , fILE_TYPE_UNKNOWN    = FILE_TYPE_UNKNOWN
 , fILE_TYPE_DISK       = FILE_TYPE_DISK
 , fILE_TYPE_CHAR       = FILE_TYPE_CHAR
 , fILE_TYPE_PIPE       = FILE_TYPE_PIPE
 , fILE_TYPE_REMOTE     = FILE_TYPE_REMOTE
 }

----------------------------------------------------------------

type LockMode = DWORD

#{enum LockMode,
 , lOCKFILE_EXCLUSIVE_LOCK   = LOCKFILE_EXCLUSIVE_LOCK
 , lOCKFILE_FAIL_IMMEDIATELY = LOCKFILE_FAIL_IMMEDIATELY
 }

----------------------------------------------------------------

newtype GET_FILEEX_INFO_LEVELS = GET_FILEEX_INFO_LEVELS (#type GET_FILEEX_INFO_LEVELS)
    deriving (Eq, Ord)

#{enum GET_FILEEX_INFO_LEVELS, GET_FILEEX_INFO_LEVELS
 , getFileExInfoStandard = GetFileExInfoStandard
 , getFileExMaxInfoLevel = GetFileExMaxInfoLevel
 }

----------------------------------------------------------------

data SECURITY_ATTRIBUTES = SECURITY_ATTRIBUTES
    { nLength              :: !DWORD
    , lpSecurityDescriptor :: !LPVOID
    , bInheritHandle       :: !BOOL
    } deriving Show

type PSECURITY_ATTRIBUTES = Ptr SECURITY_ATTRIBUTES
type LPSECURITY_ATTRIBUTES = Ptr SECURITY_ATTRIBUTES
type MbLPSECURITY_ATTRIBUTES = Maybe LPSECURITY_ATTRIBUTES

instance Storable SECURITY_ATTRIBUTES where
    sizeOf = const #{size SECURITY_ATTRIBUTES}
    alignment _ = #alignment SECURITY_ATTRIBUTES
    poke buf input = do
        (#poke SECURITY_ATTRIBUTES, nLength)              buf (nLength input)
        (#poke SECURITY_ATTRIBUTES, lpSecurityDescriptor) buf (lpSecurityDescriptor input)
        (#poke SECURITY_ATTRIBUTES, bInheritHandle)       buf (bInheritHandle input)
    peek buf = do
        nLength'              <- (#peek SECURITY_ATTRIBUTES, nLength)              buf
        lpSecurityDescriptor' <- (#peek SECURITY_ATTRIBUTES, lpSecurityDescriptor) buf
        bInheritHandle'       <- (#peek SECURITY_ATTRIBUTES, bInheritHandle)       buf
        return $ SECURITY_ATTRIBUTES nLength' lpSecurityDescriptor' bInheritHandle'

----------------------------------------------------------------
-- Other types
----------------------------------------------------------------

data BY_HANDLE_FILE_INFORMATION = BY_HANDLE_FILE_INFORMATION
    { bhfiFileAttributes :: FileAttributeOrFlag
    , bhfiCreationTime, bhfiLastAccessTime, bhfiLastWriteTime :: FILETIME
    , bhfiVolumeSerialNumber :: DWORD
    , bhfiSize :: DDWORD
    , bhfiNumberOfLinks :: DWORD
    , bhfiFileIndex :: DDWORD
    } deriving (Show)

instance Storable BY_HANDLE_FILE_INFORMATION where
    sizeOf = const (#size BY_HANDLE_FILE_INFORMATION)
    alignment _ = #alignment BY_HANDLE_FILE_INFORMATION
    poke buf bhi = do
        (#poke BY_HANDLE_FILE_INFORMATION, dwFileAttributes)     buf (bhfiFileAttributes bhi)
        (#poke BY_HANDLE_FILE_INFORMATION, ftCreationTime)       buf (bhfiCreationTime bhi)
        (#poke BY_HANDLE_FILE_INFORMATION, ftLastAccessTime)     buf (bhfiLastAccessTime bhi)
        (#poke BY_HANDLE_FILE_INFORMATION, ftLastWriteTime)      buf (bhfiLastWriteTime bhi)
        (#poke BY_HANDLE_FILE_INFORMATION, dwVolumeSerialNumber) buf (bhfiVolumeSerialNumber bhi)
        (#poke BY_HANDLE_FILE_INFORMATION, nFileSizeHigh)        buf sizeHi
        (#poke BY_HANDLE_FILE_INFORMATION, nFileSizeLow)         buf sizeLow
        (#poke BY_HANDLE_FILE_INFORMATION, nNumberOfLinks)       buf (bhfiNumberOfLinks bhi)
        (#poke BY_HANDLE_FILE_INFORMATION, nFileIndexHigh)       buf idxHi
        (#poke BY_HANDLE_FILE_INFORMATION, nFileIndexLow)        buf idxLow
        where
            (sizeHi,sizeLow) = ddwordToDwords $ bhfiSize bhi
            (idxHi,idxLow) = ddwordToDwords $ bhfiFileIndex bhi

    peek buf = do
        attr <- (#peek BY_HANDLE_FILE_INFORMATION, dwFileAttributes)     buf
        ctim <- (#peek BY_HANDLE_FILE_INFORMATION, ftCreationTime)       buf
        lati <- (#peek BY_HANDLE_FILE_INFORMATION, ftLastAccessTime)     buf
        lwti <- (#peek BY_HANDLE_FILE_INFORMATION, ftLastWriteTime)      buf
        vser <- (#peek BY_HANDLE_FILE_INFORMATION, dwVolumeSerialNumber) buf
        fshi <- (#peek BY_HANDLE_FILE_INFORMATION, nFileSizeHigh)        buf
        fslo <- (#peek BY_HANDLE_FILE_INFORMATION, nFileSizeLow)         buf
        link <- (#peek BY_HANDLE_FILE_INFORMATION, nNumberOfLinks)       buf
        idhi <- (#peek BY_HANDLE_FILE_INFORMATION, nFileIndexHigh)       buf
        idlo <- (#peek BY_HANDLE_FILE_INFORMATION, nFileIndexLow)        buf
        return $ BY_HANDLE_FILE_INFORMATION attr ctim lati lwti vser
            (dwordsToDdword (fshi,fslo)) link (dwordsToDdword (idhi,idlo))

----------------------------------------------------------------

data WIN32_FILE_ATTRIBUTE_DATA = WIN32_FILE_ATTRIBUTE_DATA
    { fadFileAttributes :: DWORD
    , fadCreationTime , fadLastAccessTime , fadLastWriteTime :: FILETIME
    , fadFileSize :: DDWORD
    } deriving (Show)

instance Storable WIN32_FILE_ATTRIBUTE_DATA where
    sizeOf = const (#size WIN32_FILE_ATTRIBUTE_DATA)
    alignment _ = #alignment WIN32_FILE_ATTRIBUTE_DATA
    poke buf ad = do
        (#poke WIN32_FILE_ATTRIBUTE_DATA, dwFileAttributes) buf (fadFileAttributes ad)
        (#poke WIN32_FILE_ATTRIBUTE_DATA, ftCreationTime)   buf (fadCreationTime ad)
        (#poke WIN32_FILE_ATTRIBUTE_DATA, ftLastAccessTime) buf (fadLastAccessTime ad)
        (#poke WIN32_FILE_ATTRIBUTE_DATA, ftLastWriteTime)  buf (fadLastWriteTime ad)
        (#poke WIN32_FILE_ATTRIBUTE_DATA, nFileSizeHigh)    buf sizeHi
        (#poke WIN32_FILE_ATTRIBUTE_DATA, nFileSizeLow)     buf sizeLo
        where
            (sizeHi,sizeLo) = ddwordToDwords $ fadFileSize ad

    peek buf = do
        attr <- (#peek WIN32_FILE_ATTRIBUTE_DATA, dwFileAttributes) buf
        ctim <- (#peek WIN32_FILE_ATTRIBUTE_DATA, ftCreationTime)   buf
        lati <- (#peek WIN32_FILE_ATTRIBUTE_DATA, ftLastAccessTime) buf
        lwti <- (#peek WIN32_FILE_ATTRIBUTE_DATA, ftLastWriteTime)  buf
        fshi <- (#peek WIN32_FILE_ATTRIBUTE_DATA, nFileSizeHigh)    buf
        fslo <- (#peek WIN32_FILE_ATTRIBUTE_DATA, nFileSizeLow)     buf
        return $ WIN32_FILE_ATTRIBUTE_DATA attr ctim lati lwti
            (dwordsToDdword (fshi,fslo))

----------------------------------------------------------------
-- File operations
----------------------------------------------------------------

foreign import WINDOWS_CCONV unsafe "windows.h DeleteFileW"
  c_DeleteFile :: LPCTSTR -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h CopyFileW"
  c_CopyFile :: LPCTSTR -> LPCTSTR -> Bool -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h MoveFileW"
  c_MoveFile :: LPCTSTR -> LPCTSTR -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h MoveFileExW"
  c_MoveFileEx :: LPCTSTR -> LPCTSTR -> MoveFileFlag -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h SetCurrentDirectoryW"
  c_SetCurrentDirectory :: LPCTSTR -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h CreateDirectoryW"
  c_CreateDirectory :: LPCTSTR -> LPSECURITY_ATTRIBUTES -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h CreateDirectoryExW"
  c_CreateDirectoryEx :: LPCTSTR -> LPCTSTR -> LPSECURITY_ATTRIBUTES -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h RemoveDirectoryW"
  c_RemoveDirectory :: LPCTSTR -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h GetBinaryTypeW"
  c_GetBinaryType :: LPCTSTR -> Ptr DWORD -> IO Bool

----------------------------------------------------------------
-- HANDLE operations
----------------------------------------------------------------

foreign import WINDOWS_CCONV unsafe "windows.h CreateFileW"
  c_CreateFile :: LPCTSTR -> AccessMode -> ShareMode -> LPSECURITY_ATTRIBUTES -> CreateMode -> FileAttributeOrFlag -> HANDLE -> IO HANDLE

foreign import WINDOWS_CCONV unsafe "windows.h CloseHandle"
  c_CloseHandle :: HANDLE -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h GetFileType"
  getFileType :: HANDLE -> IO FileType
--Apparently no error code

foreign import WINDOWS_CCONV unsafe "windows.h FlushFileBuffers"
  c_FlushFileBuffers :: HANDLE -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h SetEndOfFile"
  c_SetEndOfFile :: HANDLE -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h SetFileAttributesW"
  c_SetFileAttributes :: LPCTSTR -> FileAttributeOrFlag -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h GetFileAttributesW"
  c_GetFileAttributes :: LPCTSTR -> IO FileAttributeOrFlag

foreign import WINDOWS_CCONV unsafe "windows.h GetFileAttributesExW"
  c_GetFileAttributesEx :: LPCTSTR -> GET_FILEEX_INFO_LEVELS -> Ptr a -> IO BOOL

foreign import WINDOWS_CCONV unsafe "windows.h GetFileInformationByHandle"
    c_GetFileInformationByHandle :: HANDLE -> Ptr BY_HANDLE_FILE_INFORMATION -> IO BOOL

----------------------------------------------------------------
-- Read/write files
----------------------------------------------------------------

-- No support for this yet
data OVERLAPPED
  = OVERLAPPED { ovl_internal     :: ULONG_PTR
               , ovl_internalHigh :: ULONG_PTR
               , ovl_offset       :: DWORD
               , ovl_offsetHigh   :: DWORD
               , ovl_hEvent       :: HANDLE
               } deriving (Show)

instance Storable OVERLAPPED where
  sizeOf = const (#size OVERLAPPED)
  alignment _ = #alignment OVERLAPPED
  poke buf ad = do
      (#poke OVERLAPPED, Internal    ) buf (ovl_internal     ad)
      (#poke OVERLAPPED, InternalHigh) buf (ovl_internalHigh ad)
      (#poke OVERLAPPED, Offset      ) buf (ovl_offset       ad)
      (#poke OVERLAPPED, OffsetHigh  ) buf (ovl_offsetHigh   ad)
      (#poke OVERLAPPED, hEvent      ) buf (ovl_hEvent       ad)

  peek buf = do
      intnl      <- (#peek OVERLAPPED, Internal    ) buf
      intnl_high <- (#peek OVERLAPPED, InternalHigh) buf
      off        <- (#peek OVERLAPPED, Offset      ) buf
      off_high   <- (#peek OVERLAPPED, OffsetHigh  ) buf
      hevnt      <- (#peek OVERLAPPED, hEvent      ) buf
      return $ OVERLAPPED intnl intnl_high off off_high hevnt

type LPOVERLAPPED = Ptr OVERLAPPED

type MbLPOVERLAPPED = Maybe LPOVERLAPPED

foreign import WINDOWS_CCONV unsafe "windows.h ReadFile"
  c_ReadFile :: HANDLE -> Ptr a -> DWORD -> Ptr DWORD -> LPOVERLAPPED -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h WriteFile"
  c_WriteFile :: HANDLE -> Ptr a -> DWORD -> Ptr DWORD -> LPOVERLAPPED -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h SetFilePointerEx"
  c_SetFilePointerEx :: HANDLE -> LARGE_INTEGER -> Ptr LARGE_INTEGER -> FilePtrDirection -> IO Bool

----------------------------------------------------------------
-- File Notifications
--
-- Use these to initialise, "increment" and close a HANDLE you can wait
-- on.
----------------------------------------------------------------

foreign import WINDOWS_CCONV unsafe "windows.h FindFirstChangeNotificationW"
  c_FindFirstChangeNotification :: LPCTSTR -> Bool -> FileNotificationFlag -> IO HANDLE

foreign import WINDOWS_CCONV unsafe "windows.h FindNextChangeNotification"
  c_FindNextChangeNotification :: HANDLE -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h FindCloseChangeNotification"
  c_FindCloseChangeNotification :: HANDLE -> IO Bool

----------------------------------------------------------------
-- Directories
----------------------------------------------------------------

type WIN32_FIND_DATA = ()

newtype FindData = FindData (ForeignPtr WIN32_FIND_DATA)

foreign import WINDOWS_CCONV unsafe "windows.h FindFirstFileW"
  c_FindFirstFile :: LPCTSTR -> Ptr WIN32_FIND_DATA -> IO HANDLE

foreign import WINDOWS_CCONV unsafe "windows.h FindNextFileW"
  c_FindNextFile :: HANDLE -> Ptr WIN32_FIND_DATA -> IO BOOL

foreign import WINDOWS_CCONV unsafe "windows.h FindClose"
  c_FindClose :: HANDLE -> IO BOOL

----------------------------------------------------------------
-- DOS Device flags
----------------------------------------------------------------

foreign import WINDOWS_CCONV unsafe "windows.h DefineDosDeviceW"
  c_DefineDosDevice :: DefineDosDeviceFlags -> LPCTSTR -> LPCTSTR -> IO Bool

----------------------------------------------------------------

-- These functions are very unusual in the Win32 API:
-- They don't return error codes

foreign import WINDOWS_CCONV unsafe "windows.h AreFileApisANSI"
  areFileApisANSI :: IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h SetFileApisToOEM"
  setFileApisToOEM :: IO ()

foreign import WINDOWS_CCONV unsafe "windows.h SetFileApisToANSI"
  setFileApisToANSI :: IO ()

foreign import WINDOWS_CCONV unsafe "windows.h SetHandleCount"
  setHandleCount :: UINT -> IO UINT

----------------------------------------------------------------

foreign import WINDOWS_CCONV unsafe "windows.h GetLogicalDrives"
  c_GetLogicalDrives :: IO DWORD

-- %fun GetDriveType :: Maybe String -> IO DriveType

foreign import WINDOWS_CCONV unsafe "windows.h GetDiskFreeSpaceW"
  c_GetDiskFreeSpace :: LPCTSTR -> Ptr DWORD -> Ptr DWORD -> Ptr DWORD -> Ptr DWORD -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h SetVolumeLabelW"
  c_SetVolumeLabel :: LPCTSTR -> LPCTSTR -> IO Bool

----------------------------------------------------------------
-- File locks
----------------------------------------------------------------

foreign import WINDOWS_CCONV unsafe "LockFileEx"
  c_LockFileEx :: HANDLE -> DWORD -> DWORD -> DWORD -> DWORD -> LPOVERLAPPED
               -> IO BOOL

foreign import WINDOWS_CCONV unsafe "UnlockFileEx"
  c_UnlockFileEx :: HANDLE -> DWORD -> DWORD -> DWORD -> LPOVERLAPPED -> IO BOOL

----------------------------------------------------------------
-- End
----------------------------------------------------------------
