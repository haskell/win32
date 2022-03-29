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

module System.Win32.WindowsString.File
    ( deleteFile
    , copyFile
    , moveFile
    , moveFileEx
    , setCurrentDirectory
    , createDirectory
    , createDirectoryEx
    , removeDirectory
    , getBinaryType
    , createFile
    , setFileAttributes
    , getFileAttributes
    , getFileAttributesExStandard
    , findFirstChangeNotification
    , getFindDataFileName
    , findFirstFile
    , defineDosDevice
    , getDiskFreeSpace
    , setVolumeLabel
    , getFileExInfoStandard
    , getFileExMaxInfoLevel
    , module System.Win32.File
    ) where

import System.Win32.File.Internal
import System.Win32.File hiding (
    deleteFile
  , copyFile
  , moveFile
  , moveFileEx
  , setCurrentDirectory
  , createDirectory
  , createDirectoryEx
  , removeDirectory
  , getBinaryType
  , createFile
  , setFileAttributes
  , getFileAttributes
  , getFileAttributesExStandard
  , findFirstChangeNotification
  , getFindDataFileName
  , findFirstFile
  , defineDosDevice
  , getDiskFreeSpace
  , setVolumeLabel
  , getFileExInfoStandard
  , getFileExMaxInfoLevel
  )
import System.Win32.WindowsString.Types
import System.OsString.Windows
import Unsafe.Coerce (unsafeCoerce)

import Foreign hiding (void)

##include "windows_cconv.h"

#include <windows.h>
#include "alignment.h"

deleteFile :: WindowsString -> IO ()
deleteFile name =
  withTString name $ \ c_name ->
    failIfFalseWithRetry_ (unwords ["DeleteFile",show name]) $
      c_DeleteFile c_name

copyFile :: WindowsString -> WindowsString -> Bool -> IO ()
copyFile src dest over =
  withTString src $ \ c_src ->
  withTString dest $ \ c_dest ->
  failIfFalseWithRetry_ (unwords ["CopyFile",show src,show dest]) $
    c_CopyFile c_src c_dest over

moveFile :: WindowsString -> WindowsString -> IO ()
moveFile src dest =
  withTString src $ \ c_src ->
  withTString dest $ \ c_dest ->
  failIfFalseWithRetry_ (unwords ["MoveFile",show src,show dest]) $
    c_MoveFile c_src c_dest

moveFileEx :: WindowsString -> Maybe WindowsString -> MoveFileFlag -> IO ()
moveFileEx src dest flags =
  withTString src $ \ c_src ->
  maybeWith withTString dest $ \ c_dest ->
  failIfFalseWithRetry_ (unwords ["MoveFileEx",show src,show dest]) $
    c_MoveFileEx c_src c_dest flags

setCurrentDirectory :: WindowsString -> IO ()
setCurrentDirectory name =
  withTString name $ \ c_name ->
  failIfFalse_ (unwords ["SetCurrentDirectory",show name]) $
    c_SetCurrentDirectory c_name

createDirectory :: WindowsString -> Maybe LPSECURITY_ATTRIBUTES -> IO ()
createDirectory name mb_attr =
  withTString name $ \ c_name ->
  failIfFalseWithRetry_ (unwords ["CreateDirectory",show name]) $
    c_CreateDirectory c_name (maybePtr mb_attr)

createDirectoryEx :: WindowsString -> WindowsString -> Maybe LPSECURITY_ATTRIBUTES -> IO ()
createDirectoryEx template name mb_attr =
  withTString template $ \ c_template ->
  withTString name $ \ c_name ->
  failIfFalseWithRetry_ (unwords ["CreateDirectoryEx",show template,show name]) $
    c_CreateDirectoryEx c_template c_name (maybePtr mb_attr)

removeDirectory :: WindowsString -> IO ()
removeDirectory name =
  withTString name $ \ c_name ->
  failIfFalseWithRetry_ (unwords ["RemoveDirectory",show name]) $
    c_RemoveDirectory c_name

getBinaryType :: WindowsString -> IO BinaryType
getBinaryType name =
  withTString name $ \ c_name ->
  alloca $ \ p_btype -> do
  failIfFalse_ (unwords ["GetBinaryType",show name]) $
    c_GetBinaryType c_name p_btype
  peek p_btype

----------------------------------------------------------------
-- HANDLE operations
----------------------------------------------------------------

createFile :: WindowsString -> AccessMode -> ShareMode -> Maybe LPSECURITY_ATTRIBUTES -> CreateMode -> FileAttributeOrFlag -> Maybe HANDLE -> IO HANDLE
createFile name access share mb_attr mode flag mb_h =
  withTString name $ \ c_name ->
  failIfWithRetry (==iNVALID_HANDLE_VALUE) (unwords ["CreateFile",show name]) $
    c_CreateFile c_name access share (maybePtr mb_attr) mode flag (maybePtr mb_h)

setFileAttributes :: WindowsString -> FileAttributeOrFlag -> IO ()
setFileAttributes name attr =
  withTString name $ \ c_name ->
  failIfFalseWithRetry_ (unwords ["SetFileAttributes",show name])
    $ c_SetFileAttributes c_name attr

getFileAttributes :: WindowsString -> IO FileAttributeOrFlag
getFileAttributes name =
  withTString name $ \ c_name ->
  failIfWithRetry (== 0xFFFFFFFF) (unwords ["GetFileAttributes",show name]) $
    c_GetFileAttributes c_name

getFileAttributesExStandard :: WindowsString -> IO WIN32_FILE_ATTRIBUTE_DATA
getFileAttributesExStandard name =  alloca $ \res -> do
  withTString name $ \ c_name ->
    failIfFalseWithRetry_ "getFileAttributesExStandard" $
      c_GetFileAttributesEx c_name (unsafeCoerce getFileExInfoStandard) res
  peek res


----------------------------------------------------------------
-- File Notifications
--
-- Use these to initialise, "increment" and close a HANDLE you can wait
-- on.
----------------------------------------------------------------

findFirstChangeNotification :: WindowsString -> Bool -> FileNotificationFlag -> IO HANDLE
findFirstChangeNotification path watch flag =
  withTString path $ \ c_path ->
  failIfNull (unwords ["FindFirstChangeNotification",show path]) $
    c_FindFirstChangeNotification c_path watch flag


----------------------------------------------------------------
-- Directories
----------------------------------------------------------------


getFindDataFileName :: FindData -> IO WindowsString
getFindDataFileName fd = case unsafeCoerce fd of
  (FindData fp) ->
    withForeignPtr fp $ \p ->
      peekTString ((# ptr WIN32_FIND_DATAW, cFileName ) p)

findFirstFile :: WindowsString -> IO (HANDLE, FindData)
findFirstFile str = do
  fp_finddata <- mallocForeignPtrBytes (# const sizeof(WIN32_FIND_DATAW) )
  withForeignPtr fp_finddata $ \p_finddata -> do
    handle <- withTString str $ \tstr -> do
                failIf (== iNVALID_HANDLE_VALUE) "findFirstFile" $
                  c_FindFirstFile tstr p_finddata
    return (handle, unsafeCoerce (FindData fp_finddata))


----------------------------------------------------------------
-- DOS Device flags
----------------------------------------------------------------

defineDosDevice :: DefineDosDeviceFlags -> WindowsString -> Maybe WindowsString -> IO ()
defineDosDevice flags name path =
  maybeWith withTString path $ \ c_path ->
  withTString name $ \ c_name ->
  failIfFalse_ "DefineDosDevice" $ c_DefineDosDevice flags c_name c_path

----------------------------------------------------------------


-- %fun GetDriveType :: Maybe String -> IO DriveType

getDiskFreeSpace :: Maybe WindowsString -> IO (DWORD,DWORD,DWORD,DWORD)
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

setVolumeLabel :: Maybe WindowsString -> Maybe WindowsString -> IO ()
setVolumeLabel path name =
  maybeWith withTString path $ \ c_path ->
  maybeWith withTString name $ \ c_name ->
  failIfFalse_ "SetVolumeLabel" $ c_SetVolumeLabel c_path c_name

----------------------------------------------------------------
-- End
----------------------------------------------------------------
