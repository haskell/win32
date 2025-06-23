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
    , getTempFileName
    , replaceFile

      -- * HANDLE operations
    , createFile
    , createFile_NoRetry
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
import Data.Maybe (fromMaybe)

##include "windows_cconv.h"

#include <windows.h>
#include "alignment.h"

----------------------------------------------------------------
-- File operations
----------------------------------------------------------------

-- | Like 'failIf', but retried on sharing violations.
-- This is necessary for many file operations; see
--   https://www.betaarchive.com/wiki/index.php/Microsoft_KB_Archive/316609
--
failIfWithRetry
  :: (a -> Bool) -- ^ Predicate to check if the result indicates failure and a retry is needed.
  -> String      -- ^ Error message to use if all retries fail.
  -> IO a        -- ^ Action to perform.
  -> IO a
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

-- | Like 'failIfWithRetry', but discards the result.
failIfWithRetry_
  :: (a -> Bool) -- ^ Predicate to check if the result indicates failure and a retry is needed.
  -> String      -- ^ Error message to use if all retries fail.
  -> IO a        -- ^ Action to perform.
  -> IO ()
failIfWithRetry_ cond msg action = void $ failIfWithRetry cond msg action

-- | Like 'failIfWithRetry_', but now the predicate is the result of the action.
failIfFalseWithRetry_
  :: String  -- ^ Error message to use if all retries fail.
  -> IO Bool -- ^ Bool action to perform.
  -> IO ()
failIfFalseWithRetry_ = failIfWithRetry_ not

-- | Deletes the specified file.
-- If the file does not exist or cannot be deleted, an exception is thrown.
--
-- Note on atomicity:
-- File deletion is not guaranteed to be atomic. For example, if the file is open
-- in another process, deletion may be delayed until all handles are closed. Also,
-- in case of a system crash or power failure, the file may remain on disk or be
-- only partially deleted.
--
-- Possible errors:
-- ERROR_FILE_NOT_FOUND      2   (0x2)
-- ERROR_PATH_NOT_FOUND      3   (0x3)
-- ERROR_ACCESS_DENIED       5   (0x5)
-- ERROR_CURRENT_DIRECTORY  16   (0x10)
-- ERROR_WRITE_PROTECT      19   (0x13)
-- ERROR_SHARING_VIOLATION  32   (0x20)
-- ERROR_LOCK_VIOLATION     33   (0x21)
-- ERROR_INVALID_PARAMETER  87   (0x57)
-- ERROR_USER_MAPPED_FILE 1224   (0x4C8)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-deletefilew
deleteFile
  :: String  -- ^ Path to the file to delete
  -> IO ()
deleteFile name =
  withFilePath name $ \ c_name ->
    failIfFalseWithRetry_ (unwords ["DeleteFile",show name]) $
      c_DeleteFile c_name

-- | Copies a file from a source path to a destination path, with an option to
-- overwrite the destination file if it exists.
-- 
-- Note on atomicity:
-- File copying is not guaranteed to be atomic. For example, if a system crash or
-- power failure occurs during the copy, the destination file may be left incomplete
-- or corrupted.
--
-- Possible errors:
-- ERROR_FILE_NOT_FOUND      2   (0x2)
-- ERROR_PATH_NOT_FOUND      3   (0x3)
-- ERROR_ACCESS_DENIED       5   (0x5)
-- ERROR_SHARING_VIOLATION  32  (0x20)
-- ERROR_FILE_EXISTS        80  (0x50)
-- ERROR_ALREADY_EXISTS    183  (0xB7)
-- ERROR_INVALID_PARAMETER  87  (0x57)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-copyfilew
copyFile
  :: String  -- ^ Path to the source file
  -> String  -- ^ Path to the destination file
  -> Bool    -- ^ True to overwrite the destination if it exists, False to prevent overwriting
  -> IO ()
copyFile src dest over =
  withFilePath src $ \ c_src ->
  withFilePath dest $ \ c_dest ->
  failIfFalseWithRetry_ (unwords ["CopyFile",show src,show dest]) $
    c_CopyFile c_src c_dest over

-- | Moves a file from a source path to a destination path.
--
-- Note on atomicity:
-- File moving is not guaranteed to be atomic. For example, if the move occurs
-- across different volumes, it is implemented as a copy followed by a delete,
-- so a system crash or error during the operation may leave the destination file
-- incomplete or the source file undeleted.
--
-- Possible errors:
-- ERROR_FILE_NOT_FOUND      2   (0x2)
-- ERROR_PATH_NOT_FOUND      3   (0x3)
-- ERROR_ACCESS_DENIED       5   (0x5)
-- ERROR_ALREADY_EXISTS    183  (0xB7)
-- ERROR_SHARING_VIOLATION  32  (0x20)
-- ERROR_NOT_SAME_DEVICE    17  (0x11)
-- ERROR_INVALID_PARAMETER  87  (0x57)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-movefilew
moveFile
  :: String  -- ^ Path to the source file
  -> String  -- ^ Path to the destination file
  -> IO ()
moveFile src dest =
  withFilePath src $ \ c_src ->
  withFilePath dest $ \ c_dest ->
  failIfFalseWithRetry_ (unwords ["MoveFile",show src,show dest]) $
    c_MoveFile c_src c_dest

-- | Moves or renames a file or directory, with additional options specified by flags.
-- The destination path can be Nothing to perform certain operations (such as deleting
-- the file, depending on the flags).
-- 
-- Note on atomicity:
-- Atomicity depends on the flags and the file system. For example, using
-- MOVEFILE_REPLACE_EXISTING with source and destination on the same volume is typically
-- atomic (rename). If MOVEFILE_COPY_ALLOWED is used or the operation is across different
-- volumes, the move is performed as a copy followed by a delete, which is not atomic.
--
-- Possible errors:
-- ERROR_FILE_NOT_FOUND      2   (0x2)
-- ERROR_PATH_NOT_FOUND      3   (0x3)
-- ERROR_ACCESS_DENIED       5   (0x5)
-- ERROR_ALREADY_EXISTS    183  (0xB7)
-- ERROR_SHARING_VIOLATION  32  (0x20)
-- ERROR_NOT_SAME_DEVICE    17  (0x11)
-- ERROR_INVALID_PARAMETER  87  (0x57)
-- ERROR_WRITE_PROTECT      19  (0x13)
-- ERROR_LOCK_VIOLATION     33  (0x21)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-movefileexw
moveFileEx
  :: String           -- ^ Path to the source file or directory
  -> Maybe String     -- ^ Path to the destination file or directory, or Nothing
  -> MoveFileFlag     -- ^ Flags that control the move operation
  -> IO ()
moveFileEx src dest flags =
  withFilePath src $ \ c_src ->
  maybeWith withFilePath dest $ \ c_dest ->
  failIfFalseWithRetry_ (unwords ["MoveFileEx",show src,show dest]) $
    c_MoveFileEx c_src c_dest flags

-- | Sets the current working directory for the process to the specified path.
--
-- Note on atomicity:
-- Setting the current directory is an atomic operation for the process, but it only
-- affects the current process and its threads. Other processes are not affected.
--
-- Possible errors:
-- ERROR_FILE_NOT_FOUND      2   (0x2)
-- ERROR_PATH_NOT_FOUND      3   (0x3)
-- ERROR_ACCESS_DENIED       5   (0x5)
-- ERROR_INVALID_PARAMETER  87   (0x57)
-- ERROR_DIRECTORY         267   (0x10B)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-setcurrentdirectory
setCurrentDirectory
  :: String  -- ^ Path to the new current directory
  -> IO ()
setCurrentDirectory name =
  withFilePath name $ \ c_name ->
  failIfFalse_ (unwords ["SetCurrentDirectory",show name]) $
    c_SetCurrentDirectory c_name

-- | Creates a new directory with the specified path and optional security attributes.
-- If the directory already exists, the operation fails.
--
-- Note on atomicity:
-- Directory creation is generally atomic at the file system level, but if a system
-- crash or power failure occurs during the operation, the directory may be left
-- in an incomplete state or not created at all.
--
-- Possible errors:
-- ERROR_ALREADY_EXISTS         183  (0xB7)
-- ERROR_PATH_NOT_FOUND           3  (0x3)
-- ERROR_ACCESS_DENIED            5  (0x5)
-- ERROR_INVALID_PARAMETER       87  (0x57)
-- ERROR_FILENAME_EXCED_RANGE   206  (0xCE)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-createdirectoryw
createDirectory
  :: String                      -- ^ Path to the new directory
  -> Maybe LPSECURITY_ATTRIBUTES -- ^ Optional security attributes, or Nothing for defaults
  -> IO ()
createDirectory name mb_attr =
  withFilePath name $ \ c_name ->
  failIfFalseWithRetry_ (unwords ["CreateDirectory",show name]) $
    c_CreateDirectory c_name (maybePtr mb_attr)

-- | Creates a new directory with the specified path, optionally copying the security
-- attributes from an existing template directory and allowing custom security attributes.
-- If the directory already exists, the operation fails.
--
-- Note on atomicity:
-- Directory creation is generally atomic at the file system level, but if a system
-- crash or power failure occurs during the operation, the directory may be left
-- in an incomplete state or not created at all.
--
-- Possible errors:
-- ERROR_ALREADY_EXISTS         183  (0xB7)
-- ERROR_PATH_NOT_FOUND           3  (0x3)
-- ERROR_ACCESS_DENIED            5  (0x5)
-- ERROR_INVALID_PARAMETER       87  (0x57)
-- ERROR_FILENAME_EXCED_RANGE   206  (0xCE)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-createdirectoryexw
createDirectoryEx
  :: String                      -- ^ Path to the template directory (to copy attributes from)
  -> String                      -- ^ Path to the new directory
  -> Maybe LPSECURITY_ATTRIBUTES -- ^ Optional security attributes, or Nothing for defaults
  -> IO ()
createDirectoryEx template name mb_attr =
  withFilePath template $ \ c_template ->
  withFilePath name $ \ c_name ->
  failIfFalseWithRetry_ (unwords ["CreateDirectoryEx",show template,show name]) $
    c_CreateDirectoryEx c_template c_name (maybePtr mb_attr)

-- | Removes (deletes) the directory at the specified path.
-- The directory must be empty for the operation to succeed.
--
-- Note on atomicity:
-- Directory removal is generally atomic at the file system level, but if a system
-- crash or power failure occurs during the operation, the directory may be left
-- partially removed or still present.
--
-- Possible errors:
-- ERROR_PATH_NOT_FOUND           3   (0x3)
-- ERROR_ACCESS_DENIED            5   (0x5)
-- ERROR_SHARING_VIOLATION       32   (0x20)
-- ERROR_INVALID_PARAMETER       87   (0x57)
-- ERROR_DIRECTORY              267   (0x10B)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-removedirectoryw
removeDirectory
  :: String  -- ^ Path to the directory to remove
  -> IO ()
removeDirectory name =
  withFilePath name $ \ c_name ->
  failIfFalseWithRetry_ (unwords ["RemoveDirectory",show name]) $
    c_RemoveDirectory c_name

-- | Retrieves the binary type of the specified executable file.
-- Returns an integer code indicating the type (e.g., 32-bit, 64-bit, DOS, etc.).
--
-- Note on atomicity:
-- This operation is atomic in the sense that it simply queries file metadata and
-- does not modify the file or its contents.
--
-- Possible errors:
-- ERROR_FILE_NOT_FOUND           2   (0x2)
-- ERROR_PATH_NOT_FOUND           3   (0x3)
-- ERROR_INVALID_PARAMETER       87   (0x57)
-- ERROR_BAD_EXE_FORMAT         193   (0xC1)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-getbinarytypew
getBinaryType
  :: String  -- ^ Path to the executable file
  -> IO BinaryType
getBinaryType name =
  withFilePath name $ \ c_name ->
  alloca $ \ p_btype -> do
  failIfFalse_ (unwords ["GetBinaryType",show name]) $
    c_GetBinaryType c_name p_btype
  peek p_btype

-- | Get a unique temporary filename. Returns the full path of the new temporary file.
--
-- Note on atomicity:
-- The creation of the temporary file is not guaranteed to be atomic. If a system
-- crash or power failure occurs during the operation, the file may not be created
-- or may be left in an incomplete state.
--
-- Possible errors:
-- ERROR_DIRECTORY              267   (0x10B)
-- ERROR_ACCESS_DENIED            5   (0x5)
-- ERROR_INVALID_PARAMETER       87   (0x57)
-- ERROR_BUFFER_OVERFLOW        111   (0x6F)
-- ERROR_FILE_EXISTS             80   (0x50)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-gettempfilenamew
getTempFileName :: String     -- ^ Directory for the temporary file (must be at most MAX_PATH - 14 characters long)
                -> String     -- ^ Prefix for the temporary file name
                -> Maybe UINT -- ^ If 'Nothing', a unique name is generated
                              --   otherwise a non-zero value is used as the unique part
                -> IO (String, UINT)
getTempFileName dir prefix unique = allocaBytes ((#const MAX_PATH) * sizeOf (undefined :: TCHAR)) $ \c_buf -> do
  uid <- withFilePath dir $ \c_dir ->
    withFilePath prefix $ \ c_prefix -> do
      failIfZero "getTempFileName" $
        c_GetTempFileNameW c_dir c_prefix (fromMaybe 0 unique) c_buf
  fname <- peekTString c_buf
  return (fname, uid)

----------------------------------------------------------------
-- HANDLE operations
----------------------------------------------------------------

-- | Opens or creates a file or device with the specified access, sharing mode,
-- security attributes, creation disposition, flags, and optional template handle.
-- Returns a HANDLE to the opened or created file or device.
--
-- Note on atomicity:
-- File creation or opening is generally atomic at the file system level, but if a
-- system crash or power failure occurs during the operation, the file may be left
-- in an incomplete state or not created at all.
--
-- Possible errors:
-- ERROR_ALREADY_EXISTS    183   (0xB7)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-createfilew
createFile
  :: String                      -- ^ Path to the file or device
  -> AccessMode                  -- ^ Desired access mode
  -> ShareMode                   -- ^ Sharing mode
  -> Maybe LPSECURITY_ATTRIBUTES -- ^ Optional security attributes
  -> CreateMode                  -- ^ Action to take on files that exist or do not exist
  -> FileAttributeOrFlag         -- ^ File or device attributes and flags
  -> Maybe HANDLE                -- ^ Optional template file handle
  -> IO HANDLE
createFile = createFile' failIfWithRetry

-- | Internal function for defining the behavior of 'createFile'.
createFile'
  :: ((HANDLE -> Bool) -> String -> IO HANDLE -> IO HANDLE)
  -> String                      -- ^ Path to the file or device
  -> AccessMode                  -- ^ Desired access mode
  -> ShareMode                   -- ^ Sharing mode
  -> Maybe LPSECURITY_ATTRIBUTES -- ^ Optional security attributes
  -> CreateMode                  -- ^ Action to take on files that exist or do not exist
  -> FileAttributeOrFlag         -- ^ File or device attributes and flags
  -> Maybe HANDLE                -- ^ Optional template file handle
  -> IO HANDLE
createFile' f name access share mb_attr mode flag mb_h =
  withFilePath name $ \ c_name ->
  f (==iNVALID_HANDLE_VALUE) (unwords ["CreateFile",show name]) $
    c_CreateFile c_name access share (maybePtr mb_attr) mode flag (maybePtr mb_h)

-- | Like createFile, but does not use failIfWithRetry. If another
-- process has the same file open, this will fail.
--
-- Note on atomicity:
-- File creation or opening is generally atomic at the file system level, but if a
-- system crash or power failure occurs during the operation, the file may be left
-- in an incomplete state or not created at all.
--
-- Possible errors:
-- ERROR_ALREADY_EXISTS    183   (0xB7)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-createfilew
createFile_NoRetry
  :: String                      -- ^ Path to the file or device
  -> AccessMode                  -- ^ Desired access mode
  -> ShareMode                   -- ^ Sharing mode
  -> Maybe LPSECURITY_ATTRIBUTES -- ^ Optional security attributes
  -> CreateMode                  -- ^ Action to take on files that exist or do not exist
  -> FileAttributeOrFlag         -- ^ File or device attributes and flags
  -> Maybe HANDLE                -- ^ Optional template file handle
  -> IO HANDLE
createFile_NoRetry = createFile' failIf

-- | Closes an open object handle, such as a file, process, thread, or other system resource.
-- After closing, the handle is no longer valid.
--
-- Note on atomicity:
-- Closing a handle is atomic with respect to the handle itself: after the call,
-- the handle is either valid or closed, with no intermediate state. However,
-- resources associated with the handle may be released asynchronously by the system.
--
-- Possible errors:
-- ERROR_FILE_NOT_FOUND           2   (0x2)
-- ERROR_PATH_NOT_FOUND           3   (0x3)
-- ERROR_ACCESS_DENIED            5   (0x5)
-- ERROR_WRITE_PROTECT           19   (0x13)
-- ERROR_INVALID_HANDLE           6   (0x6)
-- ERROR_SHARING_VIOLATION       32   (0x20)
-- ERROR_FILE_EXISTS             80   (0x50)
-- ERROR_ALREADY_EXISTS         183   (0xB7)
-- ERROR_INVALID_PARAMETER       87   (0x57)
-- ERROR_INVALID_NAME           123   (0x7B)
-- ERROR_DISK_FULL              112   (0x70)
-- ERROR_DIRECTORY              267   (0x10B)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/handleapi/nf-handleapi-closehandle
closeHandle
  :: HANDLE  -- ^ Handle to be closed
  -> IO ()
closeHandle h =
  failIfFalse_ "CloseHandle" $ c_CloseHandle h

-- | Flushes the buffers of a file to disk, ensuring that all buffered data for the
-- specified file handle is written to the storage device.
--
-- Note on atomicity:
-- Flushing file buffers is atomic with respect to the flush request: all data buffered
-- for the file is written to disk as a single operation. However, if a system crash or
-- power failure occurs during the flush, some data may not be fully written.
--
-- Possible errors:
-- ERROR_INVALID_HANDLE    6   (0x6)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-flushfilebuffers
flushFileBuffers
  :: HANDLE  -- ^ Handle to the file whose buffers are to be flushed
  -> IO ()
flushFileBuffers h =
  failIfFalse_ "FlushFileBuffers" $ c_FlushFileBuffers h

-- | Sets the end of the file at the current file pointer position for the specified handle.
-- This truncates or extends the file as needed.
--
-- Note on atomicity:
-- Setting the end of file is generally atomic at the file system level. However, if a
-- system crash or power failure occurs during the operation, the file may be left in an
-- inconsistent or partially updated state.
--
-- Possible errors:
-- ERROR_INVALID_HANDLE    6   (0x6)
-- ERROR_ACCESS_DENIED     5   (0x5)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-setendoffile
setEndOfFile
  :: HANDLE  -- ^ Handle to the file
  -> IO ()
setEndOfFile h =
  failIfFalse_ "SetEndOfFile" $ c_SetEndOfFile h

-- | Sets the attributes for the specified file or directory.
-- Common attributes include read-only, hidden, system, etc.
--
-- Note on atomicity:
-- Setting file attributes is generally atomic at the file system level. However, if a
-- system crash or power failure occurs during the operation, the attributes may not be
-- updated or may be left in an inconsistent state.
--
-- Possible errors:
-- ERROR_FILE_NOT_FOUND            2   (0x2)
-- ERROR_PATH_NOT_FOUND            3   (0x3)
-- ERROR_ACCESS_DENIED             5   (0x5)
-- ERROR_INVALID_PARAMETER        87   (0x57)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-setfileattributesw
setFileAttributes
  :: String               -- ^ Path to the file or directory
  -> FileAttributeOrFlag  -- ^ Attributes to set
  -> IO ()
setFileAttributes name attr =
  withFilePath name $ \ c_name ->
  failIfFalseWithRetry_ (unwords ["SetFileAttributes",show name])
    $ c_SetFileAttributes c_name attr

-- | Retrieves the attributes of the specified file or directory.
-- Returns a set of flags indicating attributes such as read-only, hidden, system, etc.
--
-- Note on atomicity:
-- Retrieving file attributes is atomic: the operation simply queries the current
-- metadata of the file or directory and does not modify any data.
--
-- Possible errors:
-- ERROR_FILE_NOT_FOUND            2   (0x2)
-- ERROR_PATH_NOT_FOUND            3   (0x3)
-- ERROR_INVALID_PARAMETER        87   (0x57)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getfileattributesw
getFileAttributes
  :: String               -- ^ Path to the file or directory
  -> IO FileAttributeOrFlag
getFileAttributes name =
  withFilePath name $ \ c_name ->
  failIfWithRetry (== 0xFFFFFFFF) (unwords ["GetFileAttributes",show name]) $
    c_GetFileAttributes c_name

-- | Retrieves extended attribute information for the specified file or directory,
-- including size, timestamps, and standard attributes.
--
-- Note on atomicity:
-- Retrieving extended file attributes is atomic: the operation only queries the current
-- metadata of the file or directory and does not modify any data.
--
-- Possible errors:
-- ERROR_FILE_NOT_FOUND            2   (0x2)
-- ERROR_PATH_NOT_FOUND            3   (0x3)
-- ERROR_INVALID_PARAMETER        87   (0x57)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getfileattributesexw
getFileAttributesExStandard
  :: String                        -- ^ Path to the file or directory
  -> IO WIN32_FILE_ATTRIBUTE_DATA  -- ^ Structure containing the attribute data
getFileAttributesExStandard name =  alloca $ \res -> do
  withFilePath name $ \ c_name ->
    failIfFalseWithRetry_ "getFileAttributesExStandard" $
      c_GetFileAttributesEx c_name getFileExInfoStandard res
  peek res

-- | Retrieves information about a file based on its handle, including attributes,
-- size, timestamps, and volume information.
--
-- Note on atomicity:
-- Retrieving file information by handle is atomic: the operation only queries the current
-- metadata of the file and does not modify any data.
--
-- Possible errors:
-- ERROR_INVALID_HANDLE            6   (0x6)
-- ERROR_ACCESS_DENIED             5   (0x5)
-- ERROR_INVALID_PARAMETER        87   (0x57)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getfileinformationbyhandle
getFileInformationByHandle
  :: HANDLE                        -- ^ Handle to the file
  -> IO BY_HANDLE_FILE_INFORMATION -- ^ Structure containing the file information
getFileInformationByHandle h = alloca $ \res -> do
  failIfFalseWithRetry_ "GetFileInformationByHandle" $ c_GetFileInformationByHandle h res
  peek res

-- | Replaces one file with another, optionally creating a backup and specifying
-- replacement options.
-- 
-- Note on atomicity:
-- File replacement is typically atomic when both files are on the same volume and
-- no special file system features interfere. If the files are on different volumes,
-- or if a system crash or power failure occurs during the operation, atomicity is
-- not guaranteed and the destination file may be left in an inconsistent state.
--
-- Possible errors:
-- ERROR_FILE_NOT_FOUND                  2   (0x2)
-- ERROR_PATH_NOT_FOUND                  3   (0x3)
-- ERROR_ACCESS_DENIED                   5   (0x5)
-- ERROR_SHARING_VIOLATION              32   (0x20)
-- ERROR_INVALID_PARAMETER              87   (0x57)
-- ERROR_UNABLE_TO_REMOVE_REPLACED    1175   (0x497)
-- ERROR_UNABLE_TO_MOVE_REPLACEMENT   1176   (0x498)
-- ERROR_UNABLE_TO_MOVE_REPLACEMENT_2 1177   (0x499)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-replacefilew
replaceFile
  :: LPCWSTR   -- ^ replacedFile: path to the file to be replaced
  -> LPCWSTR   -- ^ replacementFile: path to the replacement file
  -> LPCWSTR   -- ^ backupFile: path to the backup file, or NULL if no backup is needed
  -> DWORD     -- ^ replaceFlags: flags that control the replacement operation
  -> IO ()
replaceFile replacedFile replacementFile backupFile replaceFlags =
  failIfFalse_ "ReplaceFile" $
    c_ReplaceFile replacedFile replacementFile backupFile replaceFlags nullPtr nullPtr

----------------------------------------------------------------
-- Read/write files
----------------------------------------------------------------

-- | Reads data from a file or input/output (I/O) device into a buffer.
-- Supports both synchronous and asynchronous (overlapped) operations.
-- Returns the number of bytes read.
--
-- Note on atomicity:
-- Each read operation is atomic with respect to the requested range: the data read
-- is either from before or after the call, with no partial modification by other
-- processes in between. However, if a system crash or power failure occurs during
-- the read, the operation may not complete or may return incomplete data.
--
-- Possible errors:
-- ERROR_ACCESS_DENIED             5   (0x5)
-- ERROR_INVALID_HANDLE            6   (0x6)
-- ERROR_HANDLE_EOF               38   (0x26)
-- ERROR_BROKEN_PIPE             109   (0x6D)
-- ERROR_INVALID_PARAMETER        87   (0x57)
-- ERROR_IO_PENDING              997   (0x3E5)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-readfile
win32_ReadFile
  :: HANDLE             -- ^ Handle to the file or I/O device
  -> Ptr a              -- ^ Pointer to the buffer that receives the data
  -> DWORD              -- ^ Number of bytes to read
  -> Maybe LPOVERLAPPED -- ^ Optional pointer to an OVERLAPPED structure for asynchronous operations
  -> IO DWORD           -- ^ Number of bytes actually read
win32_ReadFile h buf n mb_over =
  alloca $ \ p_n -> do
  failIfFalse_ "ReadFile" $ c_ReadFile h buf n p_n (maybePtr mb_over)
  peek p_n

-- | Writes data to a file or input/output (I/O) device from a buffer.
-- Supports both synchronous and asynchronous (overlapped) operations.
-- Returns the number of bytes written.
--
-- Note on atomicity:
-- Each write operation is atomic with respect to the requested range: the data is
-- either fully written or not written at all for the specified region. However, if a
-- system crash or power failure occurs during the write, the file may be left in an
-- incomplete or inconsistent state.
--
-- Possible errors:
-- ERROR_ACCESS_DENIED             5   (0x5)
-- ERROR_INVALID_HANDLE            6   (0x6)
-- ERROR_HANDLE_EOF               38   (0x26)
-- ERROR_BROKEN_PIPE             109   (0x6D)
-- ERROR_INVALID_PARAMETER        87   (0x57)
-- ERROR_DISK_FULL               112   (0x70)
-- ERROR_IO_PENDING              997   (0x3E5)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-writefile
win32_WriteFile
  :: HANDLE             -- ^ Handle to the file or I/O device
  -> Ptr a              -- ^ Pointer to the buffer containing the data to write
  -> DWORD              -- ^ Number of bytes to write
  -> Maybe LPOVERLAPPED -- ^ Optional pointer to an OVERLAPPED structure for asynchronous operations
  -> IO DWORD           -- ^ Number of bytes actually written
win32_WriteFile h buf n mb_over =
  alloca $ \ p_n -> do
  failIfFalse_ "WriteFile" $ c_WriteFile h buf n p_n (maybePtr mb_over)
  peek p_n

-- | Moves the file pointer of the specified file handle to a new position.
-- The position is set relative to the beginning, current position, or end of the file,
-- depending on the direction argument. Returns the new file pointer position.
--
-- Note on atomicity:
-- Setting the file pointer is atomic with respect to the handle: the position is
-- updated in a single operation. However, this does not affect the file contents,
-- and a system crash or power failure during subsequent operations may still cause
-- inconsistencies.
--
-- Possible errors:
-- ERROR_INVALID_HANDLE            6   (0x6)
-- ERROR_INVALID_PARAMETER        87   (0x57)
-- ERROR_NEGATIVE_SEEK           131   (0x83)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-setfilepointerex
setFilePointerEx
  :: HANDLE           -- ^ Handle to the file
  -> LARGE_INTEGER    -- ^ Distance to move the file pointer
  -> FilePtrDirection -- ^ Reference point for the move (beginning, current, or end)
  -> IO LARGE_INTEGER -- ^ New file pointer position
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

-- | Creates a change notification handle for a directory, allowing you to monitor
-- changes to files or subdirectories. Returns a handle that can be used with
-- wait functions to detect changes.
--
-- Note on atomicity:
-- The creation of a change notification handle is atomic with respect to the request:
-- the handle is either created and ready to monitor changes, or the operation fails.
-- However, there may be a delay between the actual file system change and its detection,
-- and some rapid changes may not be observed if they occur between polling intervals.
--
-- Possible errors:
-- ERROR_INVALID_PARAMETER        87   (0x57)
-- ERROR_PATH_NOT_FOUND            3   (0x3)
-- ERROR_ACCESS_DENIED             5   (0x5)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-findfirstchangenotificationw
findFirstChangeNotification
  :: String               -- ^ Path to the directory to monitor
  -> Bool                 -- ^ True to monitor subdirectories, False otherwise
  -> FileNotificationFlag -- ^ Filter specifying which changes to monitor
  -> IO HANDLE            -- ^ Notification handle
findFirstChangeNotification path watch flag =
  withFilePath path $ \ c_path ->
  failIfNull (unwords ["FindFirstChangeNotification",show path]) $
    c_FindFirstChangeNotification c_path watch flag

-- | Requests the next change notification for a directory being monitored.
-- Use after a previous change has been detected to continue monitoring.
--
-- Note on atomicity:
-- Requesting the next change notification is atomic with respect to the handle:
-- the monitoring state is updated in a single operation. However, there may be a delay
-- between the actual file system change and its detection, and some rapid changes may
-- not be observed if they occur between notifications.
--
-- Possible errors:
-- ERROR_INVALID_HANDLE    6   (0x6)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-findnextchangenotification
findNextChangeNotification
  :: HANDLE  -- ^ Notification handle returned by findFirstChangeNotification
  -> IO ()
findNextChangeNotification h =
  failIfFalse_ "FindNextChangeNotification" $ c_FindNextChangeNotification h

-- | Closes a change notification handle and releases associated system resources.
--
-- Note on atomicity:
-- Closing a change notification handle is atomic with respect to the handle: after the call,
-- the handle is either valid or closed, with no intermediate state. This does not affect
-- the state of the monitored directory or pending notifications.
--
-- Possible errors:
-- ERROR_INVALID_HANDLE    6   (0x6)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-findclosechangenotification
findCloseChangeNotification
  :: HANDLE  -- ^ Notification handle to close
  -> IO ()
findCloseChangeNotification h =
  failIfFalse_ "FindCloseChangeNotification" $ c_FindCloseChangeNotification h

----------------------------------------------------------------
-- Directories
----------------------------------------------------------------

-- | Extracts the file name from a FindData structure, which is typically obtained
-- from file search operations (e.g., FindFirstFile/FindNextFile).
--
-- Note on atomicity:
-- Extracting the file name from a FindData structure is atomic: it simply reads
-- the value from memory and does not modify any file system state.
getFindDataFileName
  :: FindData   -- ^ FindData structure containing file information
  -> IO FilePath
getFindDataFileName (FindData fp) =
  withForeignPtr fp $ \p ->
    peekTString ((# ptr WIN32_FIND_DATAW, cFileName ) p)

-- | Begins a file search in a directory that matches a given pattern (e.g., "*.txt").
-- Returns a tuple containing a search handle and a FindData structure with information
-- about the first matching file. The handle should be closed with findClose when done.
--
-- Note on atomicity:
-- Starting a file search is atomic with respect to the creation of the search handle:
-- the handle is either created and ready for enumeration, or the operation fails.
-- The file system contents may change during enumeration, so results may not reflect
-- a consistent snapshot if files are added or removed during the search.
--
-- Possible errors:
-- ERROR_FILE_NOT_FOUND            2   (0x2)
-- ERROR_PATH_NOT_FOUND            3   (0x3)
-- ERROR_ACCESS_DENIED             5   (0x5)
-- ERROR_INVALID_PARAMETER        87   (0x57)
-- ERROR_INVALID_HANDLE            6   (0x6)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-findfirstfilew
findFirstFile
  :: String                -- ^ Search pattern (e.g., file name or wildcard)
  -> IO (HANDLE, FindData) -- ^ Search handle and data for the first matching file
findFirstFile str = do
  fp_finddata <- mallocForeignPtrBytes (# const sizeof(WIN32_FIND_DATAW) )
  withForeignPtr fp_finddata $ \p_finddata -> do
    handle <- withFilePath str $ \tstr -> do
                failIf (== iNVALID_HANDLE_VALUE) "findFirstFile" $
                  c_FindFirstFile tstr p_finddata
    return (handle, FindData fp_finddata)

-- | Continues a file search from a previous findFirstFile call.
-- Updates the FindData structure with information about the next matching file.
-- Returns True if a file is found, or False if there are no more files.
-- Throws an exception if another error occurs.
--
-- Note on atomicity:
-- Retrieving the next file in a search is atomic with respect to the search handle:
-- each call returns information about a single file or signals completion. However,
-- the file system may change between calls, so the enumeration may not reflect a
-- consistent snapshot if files are added or removed during the search.
--
-- Possible errors:
-- ERROR_NO_MORE_FILES            18   (0x12)
-- ERROR_INVALID_HANDLE            6   (0x6)
-- ERROR_ACCESS_DENIED             5   (0x5)
-- ERROR_INVALID_PARAMETER        87   (0x57)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-findnextfilew
findNextFile
  :: HANDLE     -- ^ Search handle from findFirstFile
  -> FindData   -- ^ FindData structure to receive information about the next file
  -> IO Bool    -- ^ True if a file is found, False if no more files
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

-- | Closes a file search handle created by findFirstFile and releases associated resources.
--
-- Note on atomicity:
-- Closing a search handle is atomic with respect to the handle: after the call,
-- the handle is either valid or closed, with no intermediate state. This does not
-- affect the state of the file system or files being enumerated.
--
-- Possible errors:
-- ERROR_INVALID_HANDLE    6   (0x6)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-findclose
findClose
  :: HANDLE  -- ^ Search handle to close
  -> IO ()
findClose h = failIfFalse_ "findClose" $ c_FindClose h

----------------------------------------------------------------
-- DOS Device flags
----------------------------------------------------------------

-- | Defines, modifies, or deletes a DOS device name (such as a drive letter or symbolic link).
-- The flags control the operation (e.g., add, remove, or update a device mapping).
-- The path argument is optional and may be Nothing when deleting a device name.
--
-- Note on atomicity:
-- Defining, modifying, or deleting a DOS device name is atomic with respect to the device mapping:
-- the mapping is either updated or not, with no intermediate state visible to other processes.
-- However, changes may not be immediately visible to all processes due to system caching.
--
-- Possible errors:
-- ERROR_INVALID_PARAMETER        87   (0x57)
-- ERROR_ACCESS_DENIED             5   (0x5)
-- ERROR_FILE_NOT_FOUND            2   (0x2)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-definedosdevicew
defineDosDevice
  :: DefineDosDeviceFlags -- ^ Flags that control the operation
  -> String               -- ^ Device name (e.g., drive letter or symbolic link)
  -> Maybe String         -- ^ Optional target path for the device name
  -> IO ()
defineDosDevice flags name path =
  maybeWith withFilePath path $ \ c_path ->
  withFilePath name $ \ c_name ->
  failIfFalse_ "DefineDosDevice" $ c_DefineDosDevice flags c_name c_path

----------------------------------------------------------------

-- | Retrieves a bitmask representing the currently available disk drives.
-- Each bit in the result corresponds to a drive letter (A: = bit 0, B: = bit 1, etc.).
--
-- Note on atomicity:
-- Retrieving the logical drives bitmask is atomic: the operation queries the current
-- state of the system and does not modify any data.
--
-- Possible errors:
-- ERROR_INVALID_FUNCTION          1   (0x1)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getlogicaldrives
getLogicalDrives
  :: IO DWORD  -- ^ Bitmask of available logical drives
getLogicalDrives =
  failIfZero "GetLogicalDrives" $ c_GetLogicalDrives

-- | Retrieves information about the amount of free and total space on a disk.
-- Returns a tuple with (sectors per cluster, bytes per sector, number of free clusters,
-- total number of clusters). The path argument specifies the root path of the disk
-- (e.g., "C:\\") or Nothing for the current disk.
--
-- Note on atomicity:
-- Retrieving disk free space information is atomic: the operation queries the current
-- state of the file system and does not modify any data.
--
-- Possible errors:
-- ERROR_INVALID_FUNCTION          1   (0x1)
-- ERROR_PATH_NOT_FOUND            3   (0x3)
-- ERROR_ACCESS_DENIED             5   (0x5)
-- ERROR_INVALID_PARAMETER        87   (0x57)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getdiskfreespacew
getDiskFreeSpace
  :: Maybe String                    -- ^ Optional root path of the disk (e.g., "C:\\") or Nothing for current disk
  -> IO (DWORD, DWORD, DWORD, DWORD) -- ^ (Sectors per cluster, bytes per sector, number of free clusters, total clusters)
getDiskFreeSpace path =
  maybeWith withFilePath path $ \ c_path ->
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

-- | Sets the volume label for a disk volume.
-- The path argument specifies the root path of the volume (e.g., "C:\\") or Nothing for the current volume.
-- The name argument specifies the new volume label, or Nothing to delete the existing label.
--
-- Note on atomicity:
-- Setting the volume label is generally atomic at the file system level. However, if a
-- system crash or power failure occurs during the operation, the label may not be updated
-- or may be left in an inconsistent state.
--
-- Possible errors:
-- ERROR_ACCESS_DENIED             5   (0x5)
-- ERROR_INVALID_PARAMETER        87   (0x57)
-- ERROR_LABEL_TOO_LONG          154   (0x9A)
-- ERROR_INVALID_NAME            123   (0x7B)
-- ERROR_DIRECTORY               267   (0x10B)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-setvolumelabelw
setVolumeLabel
  :: Maybe String  -- ^ Optional root path of the volume (e.g., "C:\\") or Nothing for current volume
  -> Maybe String  -- ^ Optional new volume label, or Nothing to delete the label
  -> IO ()
setVolumeLabel path name =
  maybeWith withFilePath path $ \ c_path ->
  maybeWith withFilePath name $ \ c_name ->
  failIfFalse_ "SetVolumeLabel" $ c_SetVolumeLabel c_path c_name

----------------------------------------------------------------
-- File locks
----------------------------------------------------------------

-- | Locks a given range in a file handle, To lock an entire file
--   use 0xFFFFFFFFFFFFFFFF for size and 0 for offset.
--
-- Note on atomicity:
-- Locking a file region is atomic with respect to the lock request: the region is either
-- locked or not, with no intermediate state. However, if a system crash or power failure
-- occurs immediately after the lock, the state of the lock may be lost.
--
-- Possible errors:
-- ERROR_INVALID_HANDLE            6   (0x6)
-- ERROR_LOCK_VIOLATION           33   (0x21)
-- ERROR_NOT_ENOUGH_MEMORY        8    (0x8)
-- ERROR_INVALID_PARAMETER        87   (0x57)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-lockfileex
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
--
-- Note on atomicity:
-- Unlocking a file region is atomic with respect to the unlock request: the region is
-- either unlocked or not, with no intermediate state. However, if a system crash or
-- power failure occurs immediately after the unlock, the state of the lock may be lost.
--
-- Possible errors:
-- ERROR_INVALID_HANDLE            6   (0x6)
-- ERROR_NOT_LOCKED              158   (0x9E)
-- ERROR_INVALID_PARAMETER        87   (0x57)
--
-- Link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-unlockfileex
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
