-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.File
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

module System.Win32.File
{-
	( AccessMode, ShareMode, CreateMode, FileAttributeOrFlag
	, CreateFile, CloseHandle, DeleteFile, CopyFile
	, MoveFileFlag, MoveFile, MoveFileEx,
	)
-}
where

import System.Win32.Types

import Foreign

#include <windows.h>

----------------------------------------------------------------
-- Enumeration types
----------------------------------------------------------------

type AccessMode   = UINT

gENERIC_NONE :: AccessMode
gENERIC_NONE = 0

#{enum AccessMode,
 , gENERIC_READ             = GENERIC_READ
 , gENERIC_WRITE            = GENERIC_WRITE
 , gENERIC_EXECUTE          = GENERIC_EXECUTE
 , gENERIC_ALL              = GENERIC_ALL
 , dELETE                   = DELETE
 , rEAD_CONTROL             = READ_CONTROL
 , wRITE_DAC                = WRITE_DAC
 , wRITE_OWNER              = WRITE_OWNER
 , sYNCHRONIZE              = SYNCHRONIZE
 , sTANDARD_RIGHTS_REQUIRED = STANDARD_RIGHTS_REQUIRED
 , sTANDARD_RIGHTS_READ     = STANDARD_RIGHTS_READ
 , sTANDARD_RIGHTS_WRITE    = STANDARD_RIGHTS_WRITE
 , sTANDARD_RIGHTS_EXECUTE  = STANDARD_RIGHTS_EXECUTE
 , sTANDARD_RIGHTS_ALL      = STANDARD_RIGHTS_ALL
 , sPECIFIC_RIGHTS_ALL      = SPECIFIC_RIGHTS_ALL
 , aCCESS_SYSTEM_SECURITY   = ACCESS_SYSTEM_SECURITY
 , mAXIMUM_ALLOWED          = MAXIMUM_ALLOWED
 }

----------------------------------------------------------------

type ShareMode   = UINT

fILE_SHARE_NONE :: ShareMode
fILE_SHARE_NONE = 0

#{enum ShareMode,
 , fILE_SHARE_READ      = FILE_SHARE_READ
 , fILE_SHARE_WRITE     = FILE_SHARE_WRITE
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
 , mOVEFILE_REPLACE_EXISTING	= MOVEFILE_REPLACE_EXISTING
 , mOVEFILE_COPY_ALLOWED	= MOVEFILE_COPY_ALLOWED
 , mOVEFILE_DELAY_UNTIL_REBOOT	= MOVEFILE_DELAY_UNTIL_REBOOT
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

type LPSECURITY_ATTRIBUTES = Ptr ()
type MbLPSECURITY_ATTRIBUTES = Maybe LPSECURITY_ATTRIBUTES

----------------------------------------------------------------
-- File operations
----------------------------------------------------------------

deleteFile :: String -> IO ()
deleteFile name =
  withTString name $ \ c_name ->
  failIfFalse_ "DeleteFile" $ c_DeleteFile c_name
foreign import stdcall unsafe "windows.h DeleteFileW"
  c_DeleteFile :: LPCTSTR -> IO Bool

copyFile :: String -> String -> Bool -> IO ()
copyFile src dest over =
  withTString src $ \ c_src ->
  withTString dest $ \ c_dest ->
  failIfFalse_ "CopyFile" $ c_CopyFile c_src c_dest over
foreign import stdcall unsafe "windows.h CopyFileW"
  c_CopyFile :: LPCTSTR -> LPCTSTR -> Bool -> IO Bool

moveFile :: String -> String -> IO ()
moveFile src dest =
  withTString src $ \ c_src ->
  withTString dest $ \ c_dest ->
  failIfFalse_ "MoveFile" $ c_MoveFile c_src c_dest
foreign import stdcall unsafe "windows.h MoveFileW"
  c_MoveFile :: LPCTSTR -> LPCTSTR -> IO Bool

moveFileEx :: String -> String -> MoveFileFlag -> IO ()
moveFileEx src dest flags =
  withTString src $ \ c_src ->
  withTString dest $ \ c_dest ->
  failIfFalse_ "MoveFileEx" $ c_MoveFileEx c_src c_dest flags
foreign import stdcall unsafe "windows.h MoveFileExW"
  c_MoveFileEx :: LPCTSTR -> LPCTSTR -> MoveFileFlag -> IO Bool

setCurrentDirectory :: String -> IO ()
setCurrentDirectory name =
  withTString name $ \ c_name ->
  failIfFalse_ "SetCurrentDirectory" $ c_SetCurrentDirectory c_name
foreign import stdcall unsafe "windows.h SetCurrentDirectoryW"
  c_SetCurrentDirectory :: LPCTSTR -> IO Bool

createDirectory :: String -> Maybe LPSECURITY_ATTRIBUTES -> IO ()
createDirectory name mb_attr =
  withTString name $ \ c_name ->
  failIfFalse_ "CreateDirectory" $ c_CreateDirectory c_name (maybePtr mb_attr)
foreign import stdcall unsafe "windows.h CreateDirectoryW"
  c_CreateDirectory :: LPCTSTR -> LPSECURITY_ATTRIBUTES -> IO Bool

createDirectoryEx :: String -> String -> Maybe LPSECURITY_ATTRIBUTES -> IO ()
createDirectoryEx template name mb_attr =
  withTString template $ \ c_template ->
  withTString name $ \ c_name ->
  failIfFalse_ "CreateDirectoryEx" $
    c_CreateDirectoryEx c_template c_name (maybePtr mb_attr)
foreign import stdcall unsafe "windows.h CreateDirectoryExW"
  c_CreateDirectoryEx :: LPCTSTR -> LPCTSTR -> LPSECURITY_ATTRIBUTES -> IO Bool

removeDirectory :: String -> IO ()
removeDirectory name =
  withTString name $ \ c_name ->
  failIfFalse_ "RemoveDirectory" $ c_RemoveDirectory c_name
foreign import stdcall unsafe "windows.h RemoveDirectoryW"
  c_RemoveDirectory :: LPCTSTR -> IO Bool

getBinaryType :: String -> IO BinaryType
getBinaryType name =
  withTString name $ \ c_name ->
  alloca $ \ p_btype -> do
  failIfFalse_ "GetBinaryType" $ c_GetBinaryType c_name p_btype
  peek p_btype
foreign import stdcall unsafe "windows.h GetBinaryTypeW"
  c_GetBinaryType :: LPCTSTR -> Ptr DWORD -> IO Bool

----------------------------------------------------------------
-- HANDLE operations
----------------------------------------------------------------

createFile :: String -> AccessMode -> ShareMode -> Maybe LPSECURITY_ATTRIBUTES -> CreateMode -> FileAttributeOrFlag -> Maybe HANDLE -> IO HANDLE
createFile name access share mb_attr mode flag mb_h =
  withTString name $ \ c_name ->
  failIfNull "CreateFile" $
    c_CreateFile c_name access share (maybePtr mb_attr) mode flag (maybePtr mb_h)
foreign import stdcall unsafe "windows.h CreateFileW"
  c_CreateFile :: LPCTSTR -> AccessMode -> ShareMode -> LPSECURITY_ATTRIBUTES -> CreateMode -> FileAttributeOrFlag -> HANDLE -> IO HANDLE

closeHandle :: HANDLE -> IO ()
closeHandle h =
  failIfFalse_ "CloseHandle" $ c_CloseHandle h
foreign import stdcall unsafe "windows.h CloseHandle"
  c_CloseHandle :: HANDLE -> IO Bool

foreign import stdcall unsafe "windows.h GetFileType"
  getFileType :: HANDLE -> IO FileType
--Apparently no error code

flushFileBuffers :: HANDLE -> IO ()
flushFileBuffers h =
  failIfFalse_ "FlushFileBuffers" $ c_FlushFileBuffers h
foreign import stdcall unsafe "windows.h FlushFileBuffers"
  c_FlushFileBuffers :: HANDLE -> IO Bool

setEndOfFile :: HANDLE -> IO ()
setEndOfFile h =
  failIfFalse_ "SetEndOfFile" $ c_SetEndOfFile h
foreign import stdcall unsafe "windows.h SetEndOfFile"
  c_SetEndOfFile :: HANDLE -> IO Bool

setFileAttributes :: String -> FileAttributeOrFlag -> IO ()
setFileAttributes name attr =
  withTString name $ \ c_name ->
  failIfFalse_ "SetFileAttributes" $ c_SetFileAttributes c_name attr
foreign import stdcall unsafe "windows.h SetFileAttributesW"
  c_SetFileAttributes :: LPCTSTR -> FileAttributeOrFlag -> IO Bool

getFileAttributes :: String -> IO FileAttributeOrFlag
getFileAttributes name =
  withTString name $ \ c_name ->
  failIf (== 0xFFFFFFFF) "GetFileAttributes" $ c_GetFileAttributes c_name
foreign import stdcall unsafe "windows.h GetFileAttributesW"
  c_GetFileAttributes :: LPCTSTR -> IO FileAttributeOrFlag

----------------------------------------------------------------
-- Read/write files
----------------------------------------------------------------

-- No support for this yet
--type OVERLAPPED =
-- (DWORD,  -- Offset
--  DWORD,  -- OffsetHigh
--  HANDLE) -- hEvent

type LPOVERLAPPED = Ptr ()

type MbLPOVERLAPPED = Maybe LPOVERLAPPED

--Sigh - I give up & prefix win32_ to the next two to avoid
-- senseless Prelude name clashes. --sof.

win32_ReadFile :: HANDLE -> Ptr a -> DWORD -> Maybe LPOVERLAPPED -> IO DWORD
win32_ReadFile h buf n mb_over =
  alloca $ \ p_n -> do
  failIfFalse_ "ReadFile" $ c_ReadFile h buf n p_n (maybePtr mb_over)
  peek p_n
foreign import stdcall unsafe "windows.h ReadFile"
  c_ReadFile :: HANDLE -> Ptr a -> DWORD -> Ptr DWORD -> LPOVERLAPPED -> IO Bool

win32_WriteFile :: HANDLE -> Ptr a -> DWORD -> Maybe LPOVERLAPPED -> IO DWORD
win32_WriteFile h buf n mb_over =
  alloca $ \ p_n -> do
  failIfFalse_ "WriteFile" $ c_WriteFile h buf n p_n (maybePtr mb_over)
  peek p_n
foreign import stdcall unsafe "windows.h WriteFile"
  c_WriteFile :: HANDLE -> Ptr a -> DWORD -> Ptr DWORD -> LPOVERLAPPED -> IO Bool

-- missing Seek functioinality; GSL ???
-- Dont have Word64; ADR
-- %fun SetFilePointer :: HANDLE -> Word64 -> FilePtrDirection -> IO Word64

----------------------------------------------------------------
-- File Notifications
--
-- Use these to initialise, "increment" and close a HANDLE you can wait
-- on.
----------------------------------------------------------------

findFirstChangeNotification :: String -> Bool -> FileNotificationFlag -> IO HANDLE
findFirstChangeNotification path watch flag =
  withTString path $ \ c_path ->
  failIfNull "FindFirstChangeNotification" $
    c_FindFirstChangeNotification c_path watch flag
foreign import stdcall unsafe "windows.h FindFirstChangeNotificationW"
  c_FindFirstChangeNotification :: LPCTSTR -> Bool -> FileNotificationFlag -> IO HANDLE

findNextChangeNotification :: HANDLE -> IO ()
findNextChangeNotification h =
  failIfFalse_ "FindNextChangeNotification" $ c_FindNextChangeNotification h
foreign import stdcall unsafe "windows.h FindNextChangeNotification"
  c_FindNextChangeNotification :: HANDLE -> IO Bool

findCloseChangeNotification :: HANDLE -> IO ()
findCloseChangeNotification h =
  failIfFalse_ "FindCloseChangeNotification" $ c_FindCloseChangeNotification h
foreign import stdcall unsafe "windows.h FindCloseChangeNotification"
  c_FindCloseChangeNotification :: HANDLE -> IO Bool

----------------------------------------------------------------
-- DOS Device flags
----------------------------------------------------------------

defineDosDevice :: DefineDosDeviceFlags -> String -> String -> IO ()
defineDosDevice flags name path =
  withTString path $ \ c_path ->
  withTString name $ \ c_name ->
  failIfFalse_ "DefineDosDevice" $ c_DefineDosDevice flags c_name c_path
foreign import stdcall unsafe "windows.h DefineDosDeviceW"
  c_DefineDosDevice :: DefineDosDeviceFlags -> LPCTSTR -> LPCTSTR -> IO Bool

----------------------------------------------------------------

-- These functions are very unusual in the Win32 API:
-- They dont return error codes

foreign import stdcall unsafe "windows.h AreFileApisANSI"
  areFileApisANSI :: IO Bool

foreign import stdcall unsafe "windows.h SetFileApisToOEM"
  setFileApisToOEM :: IO ()

foreign import stdcall unsafe "windows.h SetFileApisToANSI"
  setFileApisToANSI :: IO ()

foreign import stdcall unsafe "windows.h SetHandleCount"
  setHandleCount :: UINT -> IO UINT

----------------------------------------------------------------

getLogicalDrives :: IO DWORD
getLogicalDrives =
  failIfZero "GetLogicalDrives" $ c_GetLogicalDrives
foreign import stdcall unsafe "windows.h GetLogicalDrives"
  c_GetLogicalDrives :: IO DWORD

-- %fun GetDriveType :: Maybe String -> IO DriveType

getDiskFreeSpace :: Maybe String -> IO (DWORD,DWORD,DWORD,DWORD)
getDiskFreeSpace path =
  withMaybePtr withTString path $ \ c_path ->
  alloca $ \ p_sectors ->
  alloca $ \ p_bytes ->
  alloca $ \ p_nfree ->
  alloca $ \ p_nclusters -> do
  failIfFalse_ "GetDiskFreeSpace" $
    c_GetDiskFreeSpace c_path p_sectors p_bytes p_nfree p_nclusters
  sectors <- peek p_sectors
  bytes <- peek p_bytes
  nfree <- peek p_nfree
  nclusters <- peek p_nclusters
  return (sectors, bytes, nfree, nclusters)
foreign import stdcall unsafe "windows.h GetDiskFreeSpaceW"
  c_GetDiskFreeSpace :: LPCTSTR -> Ptr DWORD -> Ptr DWORD -> Ptr DWORD -> Ptr DWORD -> IO Bool

setVolumeLabel :: String -> String -> IO ()
setVolumeLabel path name =
  withTString path $ \ c_path ->
  withTString name $ \ c_name ->
  failIfFalse_ "SetVolumeLabel" $ c_SetVolumeLabel c_path c_name
foreign import stdcall unsafe "windows.h SetVolumeLabelW"
  c_SetVolumeLabel :: LPCTSTR -> LPCTSTR -> IO Bool

----------------------------------------------------------------
-- End
----------------------------------------------------------------
