#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.File
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

module System.Win32.File
    ( -- * Access modes
      AccessMode
    , gENERIC_NONE
    , gENERIC_READ
    , gENERIC_WRITE
    , gENERIC_EXECUTE
    , gENERIC_ALL
    , dELETE
    , rEAD_CONTROL
    , wRITE_DAC
    , wRITE_OWNER
    , sYNCHRONIZE
    , sTANDARD_RIGHTS_REQUIRED
    , sTANDARD_RIGHTS_READ
    , sTANDARD_RIGHTS_WRITE
    , sTANDARD_RIGHTS_EXECUTE
    , sTANDARD_RIGHTS_ALL
    , sPECIFIC_RIGHTS_ALL
    , aCCESS_SYSTEM_SECURITY
    , mAXIMUM_ALLOWED
    , fILE_ADD_FILE
    , fILE_ADD_SUBDIRECTORY
    , fILE_ALL_ACCESS
    , fILE_APPEND_DATA
    , fILE_CREATE_PIPE_INSTANCE
    , fILE_DELETE_CHILD
    , fILE_EXECUTE
    , fILE_LIST_DIRECTORY
    , fILE_READ_ATTRIBUTES
    , fILE_READ_DATA
    , fILE_READ_EA
    , fILE_TRAVERSE
    , fILE_WRITE_ATTRIBUTES
    , fILE_WRITE_DATA
    , fILE_WRITE_EA

      -- * Sharing modes
    , ShareMode
    , fILE_SHARE_NONE
    , fILE_SHARE_READ
    , fILE_SHARE_WRITE
    , fILE_SHARE_DELETE

      -- * Creation modes
    , CreateMode
    , cREATE_NEW
    , cREATE_ALWAYS
    , oPEN_EXISTING
    , oPEN_ALWAYS
    , tRUNCATE_EXISTING

      -- * File attributes and flags
    , FileAttributeOrFlag
    , fILE_ATTRIBUTE_READONLY
    , fILE_ATTRIBUTE_HIDDEN
    , fILE_ATTRIBUTE_SYSTEM
    , fILE_ATTRIBUTE_DIRECTORY
    , fILE_ATTRIBUTE_ARCHIVE
    , fILE_ATTRIBUTE_NORMAL
    , fILE_ATTRIBUTE_TEMPORARY
    , fILE_ATTRIBUTE_COMPRESSED
    , fILE_ATTRIBUTE_REPARSE_POINT
    , fILE_FLAG_WRITE_THROUGH
    , fILE_FLAG_OVERLAPPED
    , fILE_FLAG_NO_BUFFERING
    , fILE_FLAG_RANDOM_ACCESS
    , fILE_FLAG_SEQUENTIAL_SCAN
    , fILE_FLAG_DELETE_ON_CLOSE
    , fILE_FLAG_BACKUP_SEMANTICS
    , fILE_FLAG_POSIX_SEMANTICS
#ifndef __WINE_WINDOWS_H
    , sECURITY_ANONYMOUS
    , sECURITY_IDENTIFICATION
    , sECURITY_IMPERSONATION
    , sECURITY_DELEGATION
    , sECURITY_CONTEXT_TRACKING
    , sECURITY_EFFECTIVE_ONLY
    , sECURITY_SQOS_PRESENT
    , sECURITY_VALID_SQOS_FLAGS
#endif

      -- * Move file flags
    , MoveFileFlag
    , mOVEFILE_REPLACE_EXISTING
    , mOVEFILE_COPY_ALLOWED
    , mOVEFILE_DELAY_UNTIL_REBOOT

      -- * File pointer directions
    , FilePtrDirection
    , fILE_BEGIN
    , fILE_CURRENT
    , fILE_END

      -- * Drive types
    , DriveType
    , dRIVE_UNKNOWN
    , dRIVE_NO_ROOT_DIR
    , dRIVE_REMOVABLE
    , dRIVE_FIXED
    , dRIVE_REMOTE
    , dRIVE_CDROM
    , dRIVE_RAMDISK

      -- * Define DOS device flags
    , DefineDosDeviceFlags
    , dDD_RAW_TARGET_PATH
    , dDD_REMOVE_DEFINITION
    , dDD_EXACT_MATCH_ON_REMOVE

      -- * Binary types
    , BinaryType
    , sCS_32BIT_BINARY
    , sCS_DOS_BINARY
    , sCS_WOW_BINARY
    , sCS_PIF_BINARY
    , sCS_POSIX_BINARY
    , sCS_OS216_BINARY

      -- * File notification flags
    , FileNotificationFlag
    , fILE_NOTIFY_CHANGE_FILE_NAME
    , fILE_NOTIFY_CHANGE_DIR_NAME
    , fILE_NOTIFY_CHANGE_ATTRIBUTES
    , fILE_NOTIFY_CHANGE_SIZE
    , fILE_NOTIFY_CHANGE_LAST_WRITE
    , fILE_NOTIFY_CHANGE_SECURITY

      -- * File types
    , FileType
    , fILE_TYPE_UNKNOWN
    , fILE_TYPE_DISK
    , fILE_TYPE_CHAR
    , fILE_TYPE_PIPE
    , fILE_TYPE_REMOTE

      -- * Lock modes
    , LockMode
    , lOCKFILE_EXCLUSIVE_LOCK
    , lOCKFILE_FAIL_IMMEDIATELY

      -- * GetFileEx information levels
    , GET_FILEEX_INFO_LEVELS
    , getFileExInfoStandard
    , getFileExMaxInfoLevel

      -- * Security attributes
    , SECURITY_ATTRIBUTES(..)
    , PSECURITY_ATTRIBUTES
    , LPSECURITY_ATTRIBUTES
    , MbLPSECURITY_ATTRIBUTES

      -- * BY_HANDLE file information
    , BY_HANDLE_FILE_INFORMATION(..)

      -- * Win32 file attribute data
    , WIN32_FILE_ATTRIBUTE_DATA(..)

      -- * Helpers
    , failIfWithRetry
    , failIfWithRetry_
    , failIfFalseWithRetry_
      -- * File operations
    , deleteFile
    , copyFile
    , moveFile
    , moveFileEx
    , setCurrentDirectory
    , createDirectory
    , createDirectoryEx
    , removeDirectory
    , getBinaryType

      -- * HANDLE operations
    , createFile
    , closeHandle
    , getFileType
    , flushFileBuffers
    , setEndOfFile
    , setFileAttributes
    , getFileAttributes
    , getFileAttributesExStandard
    , getFileInformationByHandle

      -- ** Reading/writing
      -- | Some operations below bear the @win32_@ prefix to avoid shadowing
      -- operations from "Prelude".
    , OVERLAPPED(..)
    , LPOVERLAPPED
    , MbLPOVERLAPPED
    , win32_ReadFile
    , win32_WriteFile
    , setFilePointerEx

      -- * File notifications
    , findFirstChangeNotification
    , findNextChangeNotification
    , findCloseChangeNotification

      -- * Directories
    , FindData
    , getFindDataFileName
    , findFirstFile
    , findNextFile
    , findClose

      -- * DOS device flags
    , defineDosDevice
    , areFileApisANSI
    , setFileApisToOEM
    , setFileApisToANSI
    , setHandleCount
    , getLogicalDrives
    , getDiskFreeSpace
    , setVolumeLabel

      -- * File locks
    , lockFile
    , unlockFile
    ) where

import System.Win32.File.Internal
import System.Win32.Types

import Foreign hiding (void)
import Control.Monad
import Control.Concurrent

##include "windows_cconv.h"

#include <windows.h>
#include "alignment.h"

----------------------------------------------------------------
-- File operations
----------------------------------------------------------------

-- | like failIfFalse_, but retried on sharing violations.
-- This is necessary for many file operations; see
--   http://support.microsoft.com/kb/316609
--
failIfWithRetry :: (a -> Bool) -> String -> IO a -> IO a
failIfWithRetry cond msg action = retryOrFail retries
  where
    delay   = 100*1000 -- in ms, we use threadDelay
    retries = 20 :: Int
      -- KB article recommends 250/5

    -- retryOrFail :: Int -> IO a
    retryOrFail times
      | times <= 0 = errorWin msg
      | otherwise  = do
         ret <- action
         if not (cond ret)
            then return ret
            else do
              err_code <- getLastError
              if err_code == (# const ERROR_SHARING_VIOLATION)
                then do threadDelay delay; retryOrFail (times - 1)
                else errorWin msg

failIfWithRetry_ :: (a -> Bool) -> String -> IO a -> IO ()
failIfWithRetry_ cond msg action = void $ failIfWithRetry cond msg action

failIfFalseWithRetry_ :: String -> IO Bool -> IO ()
failIfFalseWithRetry_ = failIfWithRetry_ not

deleteFile :: String -> IO ()
deleteFile name =
  withTString name $ \ c_name ->
    failIfFalseWithRetry_ (unwords ["DeleteFile",show name]) $
      c_DeleteFile c_name

copyFile :: String -> String -> Bool -> IO ()
copyFile src dest over =
  withTString src $ \ c_src ->
  withTString dest $ \ c_dest ->
  failIfFalseWithRetry_ (unwords ["CopyFile",show src,show dest]) $
    c_CopyFile c_src c_dest over

moveFile :: String -> String -> IO ()
moveFile src dest =
  withTString src $ \ c_src ->
  withTString dest $ \ c_dest ->
  failIfFalseWithRetry_ (unwords ["MoveFile",show src,show dest]) $
    c_MoveFile c_src c_dest

moveFileEx :: String -> Maybe String -> MoveFileFlag -> IO ()
moveFileEx src dest flags =
  withTString src $ \ c_src ->
  maybeWith withTString dest $ \ c_dest ->
  failIfFalseWithRetry_ (unwords ["MoveFileEx",show src,show dest]) $
    c_MoveFileEx c_src c_dest flags

setCurrentDirectory :: String -> IO ()
setCurrentDirectory name =
  withTString name $ \ c_name ->
  failIfFalse_ (unwords ["SetCurrentDirectory",show name]) $
    c_SetCurrentDirectory c_name

createDirectory :: String -> Maybe LPSECURITY_ATTRIBUTES -> IO ()
createDirectory name mb_attr =
  withTString name $ \ c_name ->
  failIfFalseWithRetry_ (unwords ["CreateDirectory",show name]) $
    c_CreateDirectory c_name (maybePtr mb_attr)

createDirectoryEx :: String -> String -> Maybe LPSECURITY_ATTRIBUTES -> IO ()
createDirectoryEx template name mb_attr =
  withTString template $ \ c_template ->
  withTString name $ \ c_name ->
  failIfFalseWithRetry_ (unwords ["CreateDirectoryEx",show template,show name]) $
    c_CreateDirectoryEx c_template c_name (maybePtr mb_attr)

removeDirectory :: String -> IO ()
removeDirectory name =
  withTString name $ \ c_name ->
  failIfFalseWithRetry_ (unwords ["RemoveDirectory",show name]) $
    c_RemoveDirectory c_name

getBinaryType :: String -> IO BinaryType
getBinaryType name =
  withTString name $ \ c_name ->
  alloca $ \ p_btype -> do
  failIfFalse_ (unwords ["GetBinaryType",show name]) $
    c_GetBinaryType c_name p_btype
  peek p_btype

----------------------------------------------------------------
-- HANDLE operations
----------------------------------------------------------------

createFile :: String -> AccessMode -> ShareMode -> Maybe LPSECURITY_ATTRIBUTES -> CreateMode -> FileAttributeOrFlag -> Maybe HANDLE -> IO HANDLE
createFile name access share mb_attr mode flag mb_h =
  withTString name $ \ c_name ->
  failIfWithRetry (==iNVALID_HANDLE_VALUE) (unwords ["CreateFile",show name]) $
    c_CreateFile c_name access share (maybePtr mb_attr) mode flag (maybePtr mb_h)

closeHandle :: HANDLE -> IO ()
closeHandle h =
  failIfFalse_ "CloseHandle" $ c_CloseHandle h

--Apparently no error code

flushFileBuffers :: HANDLE -> IO ()
flushFileBuffers h =
  failIfFalse_ "FlushFileBuffers" $ c_FlushFileBuffers h

setEndOfFile :: HANDLE -> IO ()
setEndOfFile h =
  failIfFalse_ "SetEndOfFile" $ c_SetEndOfFile h

setFileAttributes :: String -> FileAttributeOrFlag -> IO ()
setFileAttributes name attr =
  withTString name $ \ c_name ->
  failIfFalseWithRetry_ (unwords ["SetFileAttributes",show name])
    $ c_SetFileAttributes c_name attr

getFileAttributes :: String -> IO FileAttributeOrFlag
getFileAttributes name =
  withTString name $ \ c_name ->
  failIfWithRetry (== 0xFFFFFFFF) (unwords ["GetFileAttributes",show name]) $
    c_GetFileAttributes c_name

getFileAttributesExStandard :: String -> IO WIN32_FILE_ATTRIBUTE_DATA
getFileAttributesExStandard name =  alloca $ \res -> do
  withTString name $ \ c_name ->
    failIfFalseWithRetry_ "getFileAttributesExStandard" $
      c_GetFileAttributesEx c_name getFileExInfoStandard res
  peek res

getFileInformationByHandle :: HANDLE -> IO BY_HANDLE_FILE_INFORMATION
getFileInformationByHandle h = alloca $ \res -> do
    failIfFalseWithRetry_ "GetFileInformationByHandle" $ c_GetFileInformationByHandle h res
    peek res

----------------------------------------------------------------
-- Read/write files
----------------------------------------------------------------

--Sigh - I give up & prefix win32_ to the next two to avoid
-- senseless Prelude name clashes. --sof.

win32_ReadFile :: HANDLE -> Ptr a -> DWORD -> Maybe LPOVERLAPPED -> IO DWORD
win32_ReadFile h buf n mb_over =
  alloca $ \ p_n -> do
  failIfFalse_ "ReadFile" $ c_ReadFile h buf n p_n (maybePtr mb_over)
  peek p_n

win32_WriteFile :: HANDLE -> Ptr a -> DWORD -> Maybe LPOVERLAPPED -> IO DWORD
win32_WriteFile h buf n mb_over =
  alloca $ \ p_n -> do
  failIfFalse_ "WriteFile" $ c_WriteFile h buf n p_n (maybePtr mb_over)
  peek p_n

setFilePointerEx :: HANDLE -> LARGE_INTEGER -> FilePtrDirection -> IO LARGE_INTEGER
setFilePointerEx h dist dir =
  alloca $ \p_pos -> do
  failIfFalse_ "SetFilePointerEx" $ c_SetFilePointerEx h dist p_pos dir
  peek p_pos

----------------------------------------------------------------
-- File Notifications
--
-- Use these to initialise, "increment" and close a HANDLE you can wait
-- on.
----------------------------------------------------------------

findFirstChangeNotification :: String -> Bool -> FileNotificationFlag -> IO HANDLE
findFirstChangeNotification path watch flag =
  withTString path $ \ c_path ->
  failIfNull (unwords ["FindFirstChangeNotification",show path]) $
    c_FindFirstChangeNotification c_path watch flag

findNextChangeNotification :: HANDLE -> IO ()
findNextChangeNotification h =
  failIfFalse_ "FindNextChangeNotification" $ c_FindNextChangeNotification h

findCloseChangeNotification :: HANDLE -> IO ()
findCloseChangeNotification h =
  failIfFalse_ "FindCloseChangeNotification" $ c_FindCloseChangeNotification h

----------------------------------------------------------------
-- Directories
----------------------------------------------------------------

getFindDataFileName :: FindData -> IO FilePath
getFindDataFileName (FindData fp) =
  withForeignPtr fp $ \p ->
    peekTString ((# ptr WIN32_FIND_DATAW, cFileName ) p)

findFirstFile :: String -> IO (HANDLE, FindData)
findFirstFile str = do
  fp_finddata <- mallocForeignPtrBytes (# const sizeof(WIN32_FIND_DATAW) )
  withForeignPtr fp_finddata $ \p_finddata -> do
    handle <- withTString str $ \tstr -> do
                failIf (== iNVALID_HANDLE_VALUE) "findFirstFile" $
                  c_FindFirstFile tstr p_finddata
    return (handle, FindData fp_finddata)

findNextFile :: HANDLE -> FindData -> IO Bool -- False -> no more files
findNextFile h (FindData finddata) = do
  withForeignPtr finddata $ \p_finddata -> do
    b <- c_FindNextFile h p_finddata
    if b
       then return True
       else do
             err_code <- getLastError
             if err_code == (# const ERROR_NO_MORE_FILES )
                then return False
                else failWith "findNextFile" err_code

findClose :: HANDLE -> IO ()
findClose h = failIfFalse_ "findClose" $ c_FindClose h

----------------------------------------------------------------
-- DOS Device flags
----------------------------------------------------------------

defineDosDevice :: DefineDosDeviceFlags -> String -> Maybe String -> IO ()
defineDosDevice flags name path =
  maybeWith withTString path $ \ c_path ->
  withTString name $ \ c_name ->
  failIfFalse_ "DefineDosDevice" $ c_DefineDosDevice flags c_name c_path

----------------------------------------------------------------

getLogicalDrives :: IO DWORD
getLogicalDrives =
  failIfZero "GetLogicalDrives" $ c_GetLogicalDrives

-- %fun GetDriveType :: Maybe String -> IO DriveType

getDiskFreeSpace :: Maybe String -> IO (DWORD,DWORD,DWORD,DWORD)
getDiskFreeSpace path =
  maybeWith withTString path $ \ c_path ->
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

setVolumeLabel :: Maybe String -> Maybe String -> IO ()
setVolumeLabel path name =
  maybeWith withTString path $ \ c_path ->
  maybeWith withTString name $ \ c_name ->
  failIfFalse_ "SetVolumeLabel" $ c_SetVolumeLabel c_path c_name

----------------------------------------------------------------
-- File locks
----------------------------------------------------------------

-- | Locks a given range in a file handle, To lock an entire file
--   use 0xFFFFFFFFFFFFFFFF for size and 0 for offset.
lockFile :: HANDLE   -- ^ CreateFile handle
         -> LockMode -- ^ Locking mode
         -> DWORD64  -- ^ Size of region to lock
         -> DWORD64  -- ^ Beginning offset of file to lock
         -> IO BOOL  -- ^ Indicates if locking was successful, if not query
                     --   getLastError.
lockFile hwnd mode size f_offset =
  do let s_low = fromIntegral (size .&. 0xFFFFFFFF)
         s_hi  = fromIntegral (size `shiftR` 32)
         o_low = fromIntegral (f_offset .&. 0xFFFFFFFF)
         o_hi  = fromIntegral (f_offset `shiftR` 32)
         ovlp  = OVERLAPPED 0 0 o_low o_hi nullPtr
     with ovlp $ \ptr -> c_LockFileEx hwnd mode 0 s_low s_hi ptr

-- | Unlocks a given range in a file handle, To unlock an entire file
--   use 0xFFFFFFFFFFFFFFFF for size and 0 for offset.
unlockFile :: HANDLE  -- ^ CreateFile handle
           -> DWORD64 -- ^ Size of region to unlock
           -> DWORD64 -- ^ Beginning offset of file to unlock
           -> IO BOOL -- ^ Indicates if unlocking was successful, if not query
                      --   getLastError.
unlockFile hwnd size f_offset =
  do let s_low = fromIntegral (size .&. 0xFFFFFFFF)
         s_hi  = fromIntegral (size `shiftR` 32)
         o_low = fromIntegral (f_offset .&. 0xFFFFFFFF)
         o_hi  = fromIntegral (f_offset `shiftR` 32)
         ovlp  = OVERLAPPED 0 0 o_low o_hi nullPtr
     with ovlp $ \ptr -> c_UnlockFileEx hwnd 0 s_low s_hi ptr

----------------------------------------------------------------
-- End
----------------------------------------------------------------
