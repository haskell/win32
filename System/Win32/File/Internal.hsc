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

type AccessMode = UINT

-- | (GENERIC_NONE = 0) No access rights.
gENERIC_NONE :: AccessMode
gENERIC_NONE = 0

-- | (GENERIC_READ = 0x80000000) Read access to the file or object.
gENERIC_READ :: AccessMode
gENERIC_READ = #const GENERIC_READ

-- | (GENERIC_WRITE = 0x40000000) Write access to the file or object.
gENERIC_WRITE :: AccessMode
gENERIC_WRITE = #const GENERIC_WRITE

-- | (GENERIC_EXECUTE = 0x20000000) Execute access to the file or object.
gENERIC_EXECUTE :: AccessMode
gENERIC_EXECUTE = #const GENERIC_EXECUTE

-- | (GENERIC_ALL = 0x10000000) All possible access rights.
gENERIC_ALL :: AccessMode
gENERIC_ALL = #const GENERIC_ALL

-- | (DELETE = 0x10000) Delete access.
dELETE :: AccessMode
dELETE = #const DELETE

-- | (READ_CONTROL = 0x20000) Read access to the security descriptor and owner.
rEAD_CONTROL :: AccessMode
rEAD_CONTROL = #const READ_CONTROL

-- | (WRITE_DAC = 0x40000) Write access to the discretionary access control list (DACL).
wRITE_DAC :: AccessMode
wRITE_DAC = #const WRITE_DAC

-- | (WRITE_OWNER = 0x80000) Write access to the owner.
wRITE_OWNER :: AccessMode
wRITE_OWNER = #const WRITE_OWNER

-- | (SYNCHRONIZE = 0x100000) Synchronize access.
sYNCHRONIZE :: AccessMode
sYNCHRONIZE = #const SYNCHRONIZE

-- | (STANDARD_RIGHTS_REQUIRED = 0xF0000) Required to request all standard rights.
sTANDARD_RIGHTS_REQUIRED :: AccessMode
sTANDARD_RIGHTS_REQUIRED = #const STANDARD_RIGHTS_REQUIRED

-- | (STANDARD_RIGHTS_READ = 0x20000) Read access to standard rights.
sTANDARD_RIGHTS_READ :: AccessMode
sTANDARD_RIGHTS_READ = #const STANDARD_RIGHTS_READ

-- | (STANDARD_RIGHTS_WRITE = 0x20000) Write access to standard rights.
sTANDARD_RIGHTS_WRITE :: AccessMode
sTANDARD_RIGHTS_WRITE = #const STANDARD_RIGHTS_WRITE

-- | (STANDARD_RIGHTS_EXECUTE = 0x20000) Execute access to standard rights.
sTANDARD_RIGHTS_EXECUTE :: AccessMode
sTANDARD_RIGHTS_EXECUTE = #const STANDARD_RIGHTS_EXECUTE

-- | (STANDARD_RIGHTS_ALL = 0x1F0000) All standard rights.
sTANDARD_RIGHTS_ALL :: AccessMode
sTANDARD_RIGHTS_ALL = #const STANDARD_RIGHTS_ALL

-- | (SPECIFIC_RIGHTS_ALL = 0xFFFF) All specific rights.
sPECIFIC_RIGHTS_ALL :: AccessMode
sPECIFIC_RIGHTS_ALL = #const SPECIFIC_RIGHTS_ALL

-- | (ACCESS_SYSTEM_SECURITY = 0x1000000) Access to the system security ACL.
aCCESS_SYSTEM_SECURITY :: AccessMode
aCCESS_SYSTEM_SECURITY = #const ACCESS_SYSTEM_SECURITY

-- | (MAXIMUM_ALLOWED = 0x2000000) Maximum allowed access.
mAXIMUM_ALLOWED :: AccessMode
mAXIMUM_ALLOWED = #const MAXIMUM_ALLOWED

-- | (FILE_ADD_FILE = 0x2) Permission to add a file to a directory.
fILE_ADD_FILE :: AccessMode
fILE_ADD_FILE = #const FILE_ADD_FILE

-- | (FILE_ADD_SUBDIRECTORY = 0x4) Permission to add a subdirectory to a directory.
fILE_ADD_SUBDIRECTORY :: AccessMode
fILE_ADD_SUBDIRECTORY = #const FILE_ADD_SUBDIRECTORY

-- | (FILE_ALL_ACCESS = 0x1F01FF) All possible access rights for a file.
fILE_ALL_ACCESS :: AccessMode
fILE_ALL_ACCESS = #const FILE_ALL_ACCESS

-- | (FILE_APPEND_DATA = 0x4) Permission to append data to a file.
fILE_APPEND_DATA :: AccessMode
fILE_APPEND_DATA = #const FILE_APPEND_DATA

-- | (FILE_CREATE_PIPE_INSTANCE = 0x4) Permission to create a pipe instance.
fILE_CREATE_PIPE_INSTANCE :: AccessMode
fILE_CREATE_PIPE_INSTANCE = #const FILE_CREATE_PIPE_INSTANCE

-- | (FILE_DELETE_CHILD = 0x40) Permission to delete a child of a directory.
fILE_DELETE_CHILD :: AccessMode
fILE_DELETE_CHILD = #const FILE_DELETE_CHILD

-- | (FILE_EXECUTE = 0x20) Permission to execute a file.
fILE_EXECUTE :: AccessMode
fILE_EXECUTE = #const FILE_EXECUTE

-- | (FILE_LIST_DIRECTORY = 0x1) Permission to list a directory's contents.
fILE_LIST_DIRECTORY :: AccessMode
fILE_LIST_DIRECTORY = #const FILE_LIST_DIRECTORY

-- | (FILE_READ_ATTRIBUTES = 0x80) Permission to read file attributes.
fILE_READ_ATTRIBUTES :: AccessMode
fILE_READ_ATTRIBUTES = #const FILE_READ_ATTRIBUTES

-- | (FILE_READ_DATA = 0x1) Permission to read file data.
fILE_READ_DATA :: AccessMode
fILE_READ_DATA = #const FILE_READ_DATA

-- | (FILE_READ_EA = 0x8) Permission to read extended attributes.
fILE_READ_EA :: AccessMode
fILE_READ_EA = #const FILE_READ_EA

-- | (FILE_TRAVERSE = 0x20) Permission to traverse a directory.
fILE_TRAVERSE :: AccessMode
fILE_TRAVERSE = #const FILE_TRAVERSE

-- | (FILE_WRITE_ATTRIBUTES = 0x100) Permission to write file attributes.
fILE_WRITE_ATTRIBUTES :: AccessMode
fILE_WRITE_ATTRIBUTES = #const FILE_WRITE_ATTRIBUTES

-- | (FILE_WRITE_DATA = 0x2) Permission to write file data.
fILE_WRITE_DATA :: AccessMode
fILE_WRITE_DATA = #const FILE_WRITE_DATA

-- | (FILE_WRITE_EA = 0x10) Permission to write extended attributes.
fILE_WRITE_EA :: AccessMode
fILE_WRITE_EA = #const FILE_WRITE_EA

----------------------------------------------------------------

type ShareMode = UINT

-- | (FILE_SHARE_NONE = 0) Disables sharing. No other process can open the file.
fILE_SHARE_NONE :: ShareMode
fILE_SHARE_NONE = 0

-- | (FILE_SHARE_READ = 0x1) Enables subsequent open operations on the file to
-- request read access.
fILE_SHARE_READ :: ShareMode
fILE_SHARE_READ = #const FILE_SHARE_READ

-- | (FILE_SHARE_WRITE = 0x2) Enables subsequent open operations on the file to
-- request write access.
fILE_SHARE_WRITE :: ShareMode
fILE_SHARE_WRITE = #const FILE_SHARE_WRITE

-- | (FILE_SHARE_DELETE = 0x4) Enables subsequent open operations on the file to
-- request delete access.
fILE_SHARE_DELETE :: ShareMode
fILE_SHARE_DELETE = #const FILE_SHARE_DELETE

----------------------------------------------------------------

type CreateMode = UINT

-- | (CREATE_NEW = 1) Creates a new file. The function fails if the file already exists.
cREATE_NEW :: CreateMode
cREATE_NEW = #const CREATE_NEW

-- | (CREATE_ALWAYS = 2) Creates a new file, always. If the file exists, it is overwritten.
cREATE_ALWAYS :: CreateMode
cREATE_ALWAYS = #const CREATE_ALWAYS

-- | (OPEN_EXISTING = 3) Opens the file if it exists. The function fails if the file does not exist.
oPEN_EXISTING :: CreateMode
oPEN_EXISTING = #const OPEN_EXISTING

-- | (OPEN_ALWAYS = 4) Opens the file if it exists; otherwise, creates a new file.
oPEN_ALWAYS :: CreateMode
oPEN_ALWAYS = #const OPEN_ALWAYS

-- | (TRUNCATE_EXISTING = 5) Opens the file and truncates it to zero length. The function fails if the file does not exist.
tRUNCATE_EXISTING :: CreateMode
tRUNCATE_EXISTING = #const TRUNCATE_EXISTING

----------------------------------------------------------------

type FileAttributeOrFlag = UINT

-- | (FILE_ATTRIBUTE_READONLY = 0x1) The file is read-only.
fILE_ATTRIBUTE_READONLY :: FileAttributeOrFlag
fILE_ATTRIBUTE_READONLY = #const FILE_ATTRIBUTE_READONLY

-- | (FILE_ATTRIBUTE_HIDDEN = 0x2) The file is hidden.
fILE_ATTRIBUTE_HIDDEN :: FileAttributeOrFlag
fILE_ATTRIBUTE_HIDDEN = #const FILE_ATTRIBUTE_HIDDEN

-- | (FILE_ATTRIBUTE_SYSTEM = 0x4) The file is a system file.
fILE_ATTRIBUTE_SYSTEM :: FileAttributeOrFlag
fILE_ATTRIBUTE_SYSTEM = #const FILE_ATTRIBUTE_SYSTEM

-- | (FILE_ATTRIBUTE_DIRECTORY = 0x10) The handle identifies a directory.
fILE_ATTRIBUTE_DIRECTORY :: FileAttributeOrFlag
fILE_ATTRIBUTE_DIRECTORY = #const FILE_ATTRIBUTE_DIRECTORY

-- | (FILE_ATTRIBUTE_ARCHIVE = 0x20) The file is marked for archiving.
fILE_ATTRIBUTE_ARCHIVE :: FileAttributeOrFlag
fILE_ATTRIBUTE_ARCHIVE = #const FILE_ATTRIBUTE_ARCHIVE

-- | (FILE_ATTRIBUTE_NORMAL = 0x80) The file has no other attributes set.
fILE_ATTRIBUTE_NORMAL :: FileAttributeOrFlag
fILE_ATTRIBUTE_NORMAL = #const FILE_ATTRIBUTE_NORMAL

-- | (FILE_ATTRIBUTE_TEMPORARY = 0x100) The file is being used for temporary storage.
fILE_ATTRIBUTE_TEMPORARY :: FileAttributeOrFlag
fILE_ATTRIBUTE_TEMPORARY = #const FILE_ATTRIBUTE_TEMPORARY

-- | (FILE_ATTRIBUTE_COMPRESSED = 0x800) The file or directory is compressed.
fILE_ATTRIBUTE_COMPRESSED :: FileAttributeOrFlag
fILE_ATTRIBUTE_COMPRESSED = #const FILE_ATTRIBUTE_COMPRESSED

-- | (FILE_ATTRIBUTE_REPARSE_POINT = 0x400) The file or directory has an associated reparse point.
fILE_ATTRIBUTE_REPARSE_POINT :: FileAttributeOrFlag
fILE_ATTRIBUTE_REPARSE_POINT = #const FILE_ATTRIBUTE_REPARSE_POINT

-- | (FILE_FLAG_WRITE_THROUGH = 0x80000000) Write operations will go directly to disk.
fILE_FLAG_WRITE_THROUGH :: FileAttributeOrFlag
fILE_FLAG_WRITE_THROUGH = #const FILE_FLAG_WRITE_THROUGH

-- | (FILE_FLAG_OVERLAPPED = 0x40000000) The file is being opened or created for asynchronous I/O.
fILE_FLAG_OVERLAPPED :: FileAttributeOrFlag
fILE_FLAG_OVERLAPPED = #const FILE_FLAG_OVERLAPPED

-- | (FILE_FLAG_NO_BUFFERING = 0x20000000) The file is being opened with no system buffering.
fILE_FLAG_NO_BUFFERING :: FileAttributeOrFlag
fILE_FLAG_NO_BUFFERING = #const FILE_FLAG_NO_BUFFERING

-- | (FILE_FLAG_RANDOM_ACCESS = 0x10000000) Access is intended to be random.
fILE_FLAG_RANDOM_ACCESS :: FileAttributeOrFlag
fILE_FLAG_RANDOM_ACCESS = #const FILE_FLAG_RANDOM_ACCESS

-- | (FILE_FLAG_SEQUENTIAL_SCAN = 0x8000000) Access is intended to be sequential from beginning to end.
fILE_FLAG_SEQUENTIAL_SCAN :: FileAttributeOrFlag
fILE_FLAG_SEQUENTIAL_SCAN = #const FILE_FLAG_SEQUENTIAL_SCAN

-- | (FILE_FLAG_DELETE_ON_CLOSE = 0x4000000) The file is to be deleted immediately after all handles are closed.
fILE_FLAG_DELETE_ON_CLOSE :: FileAttributeOrFlag
fILE_FLAG_DELETE_ON_CLOSE = #const FILE_FLAG_DELETE_ON_CLOSE

-- | (FILE_FLAG_BACKUP_SEMANTICS = 0x2000000) The file is being opened or created for backup or restore.
fILE_FLAG_BACKUP_SEMANTICS :: FileAttributeOrFlag
fILE_FLAG_BACKUP_SEMANTICS = #const FILE_FLAG_BACKUP_SEMANTICS

-- | (FILE_FLAG_POSIX_SEMANTICS = 0x1000000) The file or directory is to be accessed according to POSIX rules.
fILE_FLAG_POSIX_SEMANTICS :: FileAttributeOrFlag
fILE_FLAG_POSIX_SEMANTICS = #const FILE_FLAG_POSIX_SEMANTICS

#ifndef __WINE_WINDOWS_H
-- | (SECURITY_ANONYMOUS = 0x0) The security anonymous level is set.
sECURITY_ANONYMOUS :: FileAttributeOrFlag
sECURITY_ANONYMOUS = #const SECURITY_ANONYMOUS

-- | (SECURITY_IDENTIFICATION = 0x10000) The security identification level is set.
sECURITY_IDENTIFICATION :: FileAttributeOrFlag
sECURITY_IDENTIFICATION = #const SECURITY_IDENTIFICATION

-- | (SECURITY_IMPERSONATION = 0x20000) The security impersonation level is set.
sECURITY_IMPERSONATION :: FileAttributeOrFlag
sECURITY_IMPERSONATION = #const SECURITY_IMPERSONATION

-- | (SECURITY_DELEGATION = 0x30000) The security delegation level is set.
sECURITY_DELEGATION :: FileAttributeOrFlag
sECURITY_DELEGATION = #const SECURITY_DELEGATION

-- | (SECURITY_CONTEXT_TRACKING = 0x40000) Enables tracking of the security context.
sECURITY_CONTEXT_TRACKING :: FileAttributeOrFlag
sECURITY_CONTEXT_TRACKING = #const SECURITY_CONTEXT_TRACKING

-- | (SECURITY_EFFECTIVE_ONLY = 0x80000) Only effective rights are used in access checks.
sECURITY_EFFECTIVE_ONLY :: FileAttributeOrFlag
sECURITY_EFFECTIVE_ONLY = #const SECURITY_EFFECTIVE_ONLY

-- | (SECURITY_SQOS_PRESENT = 0x100000) The security quality of service (SQOS) is present.
sECURITY_SQOS_PRESENT :: FileAttributeOrFlag
sECURITY_SQOS_PRESENT = #const SECURITY_SQOS_PRESENT

-- | (SECURITY_VALID_SQOS_FLAGS = 0x1F0000) All valid SQOS flags.
sECURITY_VALID_SQOS_FLAGS :: FileAttributeOrFlag
sECURITY_VALID_SQOS_FLAGS = #const SECURITY_VALID_SQOS_FLAGS
#endif

----------------------------------------------------------------

type MoveFileFlag = DWORD

-- | (MOVEFILE_REPLACE_EXISTING = 0x1) If the destination file already
-- exists, replace it.
mOVEFILE_REPLACE_EXISTING :: MoveFileFlag
mOVEFILE_REPLACE_EXISTING = #const MOVEFILE_REPLACE_EXISTING

-- | (MOVEFILE_COPY_ALLOWED = 0x2) Allow the move to be performed by
-- copying and deleting if necessary.
mOVEFILE_COPY_ALLOWED :: MoveFileFlag
mOVEFILE_COPY_ALLOWED = #const MOVEFILE_COPY_ALLOWED

-- | (MOVEFILE_DELAY_UNTIL_REBOOT = 0x4) Delay the move operation until
-- the system reboots.
mOVEFILE_DELAY_UNTIL_REBOOT :: MoveFileFlag
mOVEFILE_DELAY_UNTIL_REBOOT = #const MOVEFILE_DELAY_UNTIL_REBOOT

-- | (MOVEFILE_FAIL_IF_NOT_TRACKABLE = 0x20) The move operation will fail if
-- the system cannot track the file or directory.
mOVEFILE_FAIL_IF_NOT_TRACKABLE :: MoveFileFlag
mOVEFILE_FAIL_IF_NOT_TRACKABLE = #const MOVEFILE_FAIL_IF_NOT_TRACKABLE

-- | (MOVEFILE_WRITE_THROUGH = 0x8) The function does not return until the file
-- move is complete, including flushing the file buffers.
mOVEFILE_WRITE_THROUGH :: MoveFileFlag
mOVEFILE_WRITE_THROUGH = #const MOVEFILE_WRITE_THROUGH

-- | (MOVEFILE_CREATE_HARDLINK = 0x10) The function creates a hard link at the
-- destination instead of moving the file.
mOVEFILE_CREATE_HARDLINK :: MoveFileFlag
mOVEFILE_CREATE_HARDLINK = #const MOVEFILE_CREATE_HARDLINK

----------------------------------------------------------------

type FilePtrDirection = DWORD

-- | (FILE_BEGIN = 0) The starting point is the beginning of the file.
fILE_BEGIN :: FilePtrDirection
fILE_BEGIN = #const FILE_BEGIN

-- | (FILE_CURRENT = 1) The starting point is the current file pointer position.
fILE_CURRENT :: FilePtrDirection
fILE_CURRENT = #const FILE_CURRENT

-- | (FILE_END = 2) The starting point is the end of the file.
fILE_END :: FilePtrDirection
fILE_END = #const FILE_END

----------------------------------------------------------------

type DriveType = UINT

-- | (DRIVE_UNKNOWN = 0) The drive type cannot be determined.
dRIVE_UNKNOWN :: DriveType
dRIVE_UNKNOWN = #const DRIVE_UNKNOWN

-- | (DRIVE_NO_ROOT_DIR = 1) The root path is invalid; for example, there is no
-- volume mounted at the path.
dRIVE_NO_ROOT_DIR :: DriveType
dRIVE_NO_ROOT_DIR = #const DRIVE_NO_ROOT_DIR

-- | (DRIVE_REMOVABLE = 2) The drive is a type that has removable media, such as
-- a floppy drive, USB, or SD card.
dRIVE_REMOVABLE :: DriveType
dRIVE_REMOVABLE = #const DRIVE_REMOVABLE

-- | (DRIVE_FIXED = 3) The drive is a fixed disk, such as a hard drive or SSD.
dRIVE_FIXED :: DriveType
dRIVE_FIXED = #const DRIVE_FIXED

-- | (DRIVE_REMOTE = 4) The drive is a remote (network) drive.
dRIVE_REMOTE :: DriveType
dRIVE_REMOTE = #const DRIVE_REMOTE

-- | (DRIVE_CDROM = 5) The drive is a CD-ROM drive.
dRIVE_CDROM :: DriveType
dRIVE_CDROM = #const DRIVE_CDROM

-- | (DRIVE_RAMDISK = 6) The drive is a RAM disk.
dRIVE_RAMDISK :: DriveType
dRIVE_RAMDISK = #const DRIVE_RAMDISK

----------------------------------------------------------------

type DefineDosDeviceFlags = DWORD

-- | (DDD_RAW_TARGET_PATH = 0x1) Uses the exact path specified by the target
-- path. Otherwise, the system converts the path.
dDD_RAW_TARGET_PATH :: DefineDosDeviceFlags
dDD_RAW_TARGET_PATH = #const DDD_RAW_TARGET_PATH

-- | (DDD_REMOVE_DEFINITION = 0x2) Removes the specified definition for the
-- device.
dDD_REMOVE_DEFINITION :: DefineDosDeviceFlags
dDD_REMOVE_DEFINITION = #const DDD_REMOVE_DEFINITION

-- | (DDD_EXACT_MATCH_ON_REMOVE = 0x4) Removes the device definition only if the
-- exact match is found.
dDD_EXACT_MATCH_ON_REMOVE :: DefineDosDeviceFlags
dDD_EXACT_MATCH_ON_REMOVE = #const DDD_EXACT_MATCH_ON_REMOVE

----------------------------------------------------------------

type BinaryType = DWORD

-- | (SCS_32BIT_BINARY = 0) Win32-based application (32-bit).
sCS_32BIT_BINARY :: BinaryType
sCS_32BIT_BINARY = #const SCS_32BIT_BINARY

-- | (SCS_DOS_BINARY = 1) MS-DOS-based application.
sCS_DOS_BINARY :: BinaryType
sCS_DOS_BINARY = #const SCS_DOS_BINARY

-- | (SCS_WOW_BINARY = 2) 16-bit Windows-based application.
sCS_WOW_BINARY :: BinaryType
sCS_WOW_BINARY = #const SCS_WOW_BINARY

-- | (SCS_PIF_BINARY = 3) PIF file that executes an MS-DOS-based application.
sCS_PIF_BINARY :: BinaryType
sCS_PIF_BINARY = #const SCS_PIF_BINARY

-- | (SCS_POSIX_BINARY = 4) POSIX-based application.
sCS_POSIX_BINARY :: BinaryType
sCS_POSIX_BINARY = #const SCS_POSIX_BINARY

-- | (SCS_OS216_BINARY = 5) 16-bit OS/2-based application.
sCS_OS216_BINARY :: BinaryType
sCS_OS216_BINARY = #const SCS_OS216_BINARY

----------------------------------------------------------------

type ReplaceType = DWORD

-- | (REPLACEFILE_WRITE_THROUGH = 0x1) Write through to the file system immediately.
rEPLACEFILE_WRITE_THROUGH :: ReplaceType
rEPLACEFILE_WRITE_THROUGH = #const REPLACEFILE_WRITE_THROUGH

-- | (REPLACEFILE_IGNORE_MERGE_ERRORS = 0x2) Ignore errors that occur while
-- merging information (such as attributes and ACLs) from the replaced file to
-- the replacement file.
rEPLACEFILE_IGNORE_MERGE_ERRORS :: ReplaceType
rEPLACEFILE_IGNORE_MERGE_ERRORS = #const REPLACEFILE_IGNORE_MERGE_ERRORS

-- | (REPLACEFILE_IGNORE_ACL_ERRORS = 0x4) Ignore errors that occur while
-- merging ACL information from the replaced file to the replacement file.
rEPLACEFILE_IGNORE_ACL_ERRORS :: ReplaceType
rEPLACEFILE_IGNORE_ACL_ERRORS = #const REPLACEFILE_IGNORE_ACL_ERRORS

----------------------------------------------------------------

type FileNotificationFlag = DWORD

-- | (FILE_NOTIFY_CHANGE_FILE_NAME = 0x1) Notify when a file name is changed in
-- the watched directory.
fILE_NOTIFY_CHANGE_FILE_NAME :: FileNotificationFlag
fILE_NOTIFY_CHANGE_FILE_NAME = #const FILE_NOTIFY_CHANGE_FILE_NAME

-- | (FILE_NOTIFY_CHANGE_DIR_NAME = 0x2) Notify when a directory name is changed
-- in the watched directory.
fILE_NOTIFY_CHANGE_DIR_NAME :: FileNotificationFlag
fILE_NOTIFY_CHANGE_DIR_NAME = #const FILE_NOTIFY_CHANGE_DIR_NAME

-- | (FILE_NOTIFY_CHANGE_ATTRIBUTES = 0x4) Notify when attributes of a file or
-- directory are changed.
fILE_NOTIFY_CHANGE_ATTRIBUTES :: FileNotificationFlag
fILE_NOTIFY_CHANGE_ATTRIBUTES = #const FILE_NOTIFY_CHANGE_ATTRIBUTES

-- | (FILE_NOTIFY_CHANGE_SIZE = 0x8) Notify when the size of a file or directory
-- is changed.
fILE_NOTIFY_CHANGE_SIZE :: FileNotificationFlag
fILE_NOTIFY_CHANGE_SIZE = #const FILE_NOTIFY_CHANGE_SIZE

-- | (FILE_NOTIFY_CHANGE_LAST_WRITE = 0x10) Notify when the last write time of a
-- file or directory is changed.
fILE_NOTIFY_CHANGE_LAST_WRITE :: FileNotificationFlag
fILE_NOTIFY_CHANGE_LAST_WRITE = #const FILE_NOTIFY_CHANGE_LAST_WRITE

-- | (FILE_NOTIFY_CHANGE_SECURITY = 0x100) Notify when the security descriptor
-- of a file or directory is changed.
fILE_NOTIFY_CHANGE_SECURITY :: FileNotificationFlag
fILE_NOTIFY_CHANGE_SECURITY = #const FILE_NOTIFY_CHANGE_SECURITY

----------------------------------------------------------------

type FileType = DWORD

-- | (FILE_TYPE_UNKNOWN = 0x0000) The type of the file is unknown.
fILE_TYPE_UNKNOWN :: FileType
fILE_TYPE_UNKNOWN = #const FILE_TYPE_UNKNOWN

-- | (FILE_TYPE_DISK = 0x0001) The file is a disk file.
fILE_TYPE_DISK :: FileType
fILE_TYPE_DISK = #const FILE_TYPE_DISK

-- | (FILE_TYPE_CHAR = 0x0002) The file is a character file, typically an LPT
-- device or a console.
fILE_TYPE_CHAR :: FileType
fILE_TYPE_CHAR = #const FILE_TYPE_CHAR

-- | (FILE_TYPE_PIPE = 0x0003) The file is a pipe or named pipe.
fILE_TYPE_PIPE :: FileType
fILE_TYPE_PIPE = #const FILE_TYPE_PIPE

-- | (FILE_TYPE_REMOTE = 0x8000) The file is a remote file.
fILE_TYPE_REMOTE :: FileType
fILE_TYPE_REMOTE = #const FILE_TYPE_REMOTE

----------------------------------------------------------------

type LockMode = DWORD

-- | (LOCKFILE_EXCLUSIVE_LOCK = 0x2) Locks the specified region of the file
-- exclusively. No other process can access the locked region.
lOCKFILE_EXCLUSIVE_LOCK :: LockMode
lOCKFILE_EXCLUSIVE_LOCK = #const LOCKFILE_EXCLUSIVE_LOCK

-- | (LOCKFILE_FAIL_IMMEDIATELY = 0x1) Fails immediately if the lock cannot be
-- acquired.
lOCKFILE_FAIL_IMMEDIATELY :: LockMode
lOCKFILE_FAIL_IMMEDIATELY = #const LOCKFILE_FAIL_IMMEDIATELY

----------------------------------------------------------------

-- | GET_FILEEX_INFO_LEVELS is used to specify the type of information to retrieve
-- in functions like GetFileAttributesExW. (for example, it may be Int on
-- 32-bit systems and DWORD on 64-bit systems, or as defined in the Windows SDK)
newtype GET_FILEEX_INFO_LEVELS = GET_FILEEX_INFO_LEVELS (#type GET_FILEEX_INFO_LEVELS)
    deriving (Eq, Ord)

-- | (GetFileExInfoStandard = 0) Retrieves the standard set of file attribute
-- information. The lpFileInformation parameter must point to a
-- WIN32_FILE_ATTRIBUTE_DATA structure.
getFileExInfoStandard :: GET_FILEEX_INFO_LEVELS
getFileExInfoStandard = GET_FILEEX_INFO_LEVELS $ #const GetFileExInfoStandard

-- | (GetFileExMaxInfoLevel = 1) Reserved for future use.
getFileExMaxInfoLevel :: GET_FILEEX_INFO_LEVELS
getFileExMaxInfoLevel = GET_FILEEX_INFO_LEVELS $ #const GetFileExMaxInfoLevel

----------------------------------------------------------------

data SECURITY_ATTRIBUTES = SECURITY_ATTRIBUTES
  { nLength              :: !DWORD   -- ^ The size, in bytes, of this structure.
  , lpSecurityDescriptor :: !LPVOID  -- ^ A pointer to a security descriptor for the object.
  , bInheritHandle       :: !BOOL    -- ^ Specifies whether the returned handle is inheritable.
  } deriving (Show)

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
  { bhfiFileAttributes     :: !FileAttributeOrFlag -- ^ File attributes.
  , bhfiCreationTime       :: !FILETIME            -- ^ Time the file was created.
  , bhfiLastAccessTime     :: !FILETIME            -- ^ Time the file was last accessed.
  , bhfiLastWriteTime      :: !FILETIME            -- ^ Time the file was last written to.
  , bhfiVolumeSerialNumber :: !DWORD               -- ^ Serial number of the volume that contains the file.
  , bhfiSize               :: !DDWORD              -- ^ File size, in bytes.
  , bhfiNumberOfLinks      :: !DWORD               -- ^ Number of hard links to the file.
  , bhfiFileIndex          :: !DDWORD              -- ^ Unique identifier for the file within the volume.
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
  { fadFileAttributes :: !DWORD     -- ^ File attributes.
  , fadCreationTime   :: !FILETIME  -- ^ Time the file was created.
  , fadLastAccessTime :: !FILETIME  -- ^ Time the file was last accessed.
  , fadLastWriteTime  :: !FILETIME  -- ^ Time the file was last written to.
  , fadFileSize       :: !DDWORD    -- ^ File size, in bytes.
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

-- | Deletes an existing file.
--------------------------------------------------------------------------------
-- Deletes the specified file. If the file is open or in use, the function fails.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] lpFileName
-- The name of the file to be deleted.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
--
-- ERROR_SHARING_VIOLATION  32  (0x20)
-- The file is being used by another process.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-deletefilew
foreign import WINDOWS_CCONV unsafe "windows.h DeleteFileW"
  c_DeleteFile :: LPCTSTR -> IO Bool

-- | Copies an existing file to a new file.
--------------------------------------------------------------------------------
-- Copies an existing file to a new file. The copy fails if the destination file
-- already exists, unless the failIfExists parameter is set to FALSE.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] lpExistingFileName
-- The name of an existing file.
--------------------------------------------------------------------------------
-- [in] lpNewFileName
-- The name of the new file.
--------------------------------------------------------------------------------
-- [in] bFailIfExists
-- If this parameter is TRUE and the new file specified by lpNewFileName already
-- exists, the function fails. If this parameter is FALSE and the new file
-- already exists, the function overwrites the existing file and succeeds.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
--
-- ERROR_FILE_EXISTS        80  (0x50)
-- The destination file already exists and bFailIfExists is TRUE.
--
-- ERROR_SHARING_VIOLATION  32  (0x20)
-- The file is being used by another process.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-copyfilew
foreign import WINDOWS_CCONV unsafe "windows.h CopyFileW"
  c_CopyFile :: LPCTSTR -> LPCTSTR -> Bool -> IO Bool

-- | Moves an existing file or directory to a new location.
--------------------------------------------------------------------------------
-- Moves an existing file or directory to a new location. The move fails if the
-- destination already exists.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] lpExistingFileName
-- The current name of the file or directory.
--------------------------------------------------------------------------------
-- [in] lpNewFileName
-- The new name for the file or directory.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
--
-- ERROR_ALREADY_EXISTS      183 (0xB7)
-- The destination already exists and cannot be replaced.
--
-- ERROR_SHARING_VIOLATION   32 (0x20)
-- The file is being used by another process.
--
-- ERROR_NOT_SAME_DEVICE     17 (0x11)
-- The file cannot be moved to a different disk drive.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-movefilew
foreign import WINDOWS_CCONV unsafe "windows.h MoveFileW"
  c_MoveFile :: LPCTSTR -> LPCTSTR -> IO Bool

-- | Moves an existing file or directory, including its children.
--------------------------------------------------------------------------------
-- Moves an existing file or directory to a new location, with the option to
-- replace the destination file or directory if it already exists.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] lpExistingFileName
-- The current name of the file or directory.
--------------------------------------------------------------------------------
-- [in] lpNewFileName
-- The new name for the file or directory.
--------------------------------------------------------------------------------
-- [in] dwFlags
-- Move options. This parameter can be one or more of the following values:
--   MOVEFILE_REPLACE_EXISTING    0x00000001
--   MOVEFILE_COPY_ALLOWED        0x00000002
--   MOVEFILE_DELAY_UNTIL_REBOOT  0x00000004
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
--
-- ERROR_ALREADY_EXISTS      183 (0xB7)
-- The destination already exists and cannot be replaced.
--
-- ERROR_SHARING_VIOLATION   32 (0x20)
-- The file is being used by another process.
--
-- ERROR_NOT_SAME_DEVICE     17 (0x11)
-- The file cannot be moved to a different disk drive.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-movefileexw
foreign import WINDOWS_CCONV unsafe "windows.h MoveFileExW"
  c_MoveFileEx :: LPCTSTR -> LPCTSTR -> MoveFileFlag -> IO Bool

-- | Changes the current directory for the current process.
--------------------------------------------------------------------------------
-- Sets the current directory for the current process to the specified path.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] lpPathName
-- The path to the new current directory.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
-- Or returns common errors.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-setcurrentdirectory
foreign import WINDOWS_CCONV unsafe "windows.h SetCurrentDirectoryW"
  c_SetCurrentDirectory :: LPCTSTR -> IO Bool

-- | Creates a new directory.
--------------------------------------------------------------------------------
-- Creates a new directory with the specified name. The security descriptor of
-- the new directory can be specified.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] lpPathName
-- The path of the directory to be created.
--------------------------------------------------------------------------------
-- [in, optional] lpSecurityAttributes
-- A pointer to a SECURITY_ATTRIBUTES structure that determines whether the
-- returned handle can be inherited by child processes. Can be NULL.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
--
-- ERROR_ALREADY_EXISTS   183 (0xB7)
-- The directory already exists.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-createdirectoryw
foreign import WINDOWS_CCONV unsafe "windows.h CreateDirectoryW"
  c_CreateDirectory :: LPCTSTR -> LPSECURITY_ATTRIBUTES -> IO Bool

-- | Creates a directory as a copy of an existing directory.
--------------------------------------------------------------------------------
-- Creates a new directory with the attributes and security descriptor of an existing directory.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] lpTemplateDirectory
-- The path of an existing directory whose attributes and security descriptor
-- are to be copied to the new directory.
--------------------------------------------------------------------------------
-- [in] lpNewDirectory
-- The path of the directory to be created.
--------------------------------------------------------------------------------
-- [in, optional] lpSecurityAttributes
-- A pointer to a SECURITY_ATTRIBUTES structure that determines whether the
-- returned handle can be inherited by child processes. Can be NULL.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
--
-- ERROR_ALREADY_EXISTS   183 (0xB7)
-- The directory already exists.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-createdirectoryexw
foreign import WINDOWS_CCONV unsafe "windows.h CreateDirectoryExW"
  c_CreateDirectoryEx :: LPCTSTR -> LPCTSTR -> LPSECURITY_ATTRIBUTES -> IO Bool

-- | Removes (deletes) an existing empty directory.
--------------------------------------------------------------------------------
-- Deletes the specified empty directory. If the directory is not empty or is in use,
-- the function fails.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] lpPathName
-- The path of the directory to be removed. The path must specify an empty directory,
-- and the calling process must have delete access to the directory.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
--
-- ERROR_DIR_NOT_EMPTY      145 (0x91)
-- The directory is not empty.
--
-- ERROR_SHARING_VIOLATION  32  (0x20)
-- The directory is being used by another process.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-removedirectoryw
foreign import WINDOWS_CCONV unsafe "windows.h RemoveDirectoryW"
  c_RemoveDirectory :: LPCTSTR -> IO Bool

-- | Determines the type of a binary executable file.
--------------------------------------------------------------------------------
-- Determines whether a file is executable, and if so, what type of executable
-- it is (e.g., console application, Windows application, etc.).
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] lpApplicationName
-- The full path of the binary executable file for which to determine the type.
--------------------------------------------------------------------------------
-- [out] lpBinaryType
-- A pointer to a variable that receives the binary type. This parameter can
-- be one of the following values:
--
-- SCS_32BIT_BINARY 0 (32-bit Windows-based application)
-- SCS_64BIT_BINARY 6 (64-bit Windows-based application)
-- SCS_DOS_BINARY   1 (MS-DOS-based application)
-- SCS_OS216_BINARY 5 (16-bit OS/2-based application) (not supported on 64-bit Windows)
-- SCS_PIF_BINARY   3 (PIF file that executes an MS-DOS-based application)
-- SCS_POSIX_BINARY 4 (POSIX-based application)
-- SCS_WOW_BINARY   2 (16-bit Windows-based application)
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
--
-- ERROR_BAD_EXE_FORMAT    193 (0xC1)
-- The specified file is not a valid executable file.
--
-- ERROR_INVALID_PARAMETER 87  (0x57)
-- The lpBinaryType parameter is NULL or the specified file is not a valid executable file.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-getbinarytypew
foreign import WINDOWS_CCONV unsafe "windows.h GetBinaryTypeW"
  c_GetBinaryType :: LPCTSTR -> Ptr DWORD -> IO Bool

-- | Replaces one file with another file.
--------------------------------------------------------------------------------
-- Replaces one file with another file, with the option of creating a backup
-- copy of the original file. The replacement file assumes the name of the
-- replaced file and its identity.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] lpReplacedFileName
-- The name of the file to be replaced.
--------------------------------------------------------------------------------
-- [in] lpReplacementFileName
-- The name of the file that will replace the lpReplacedFileName file.
--------------------------------------------------------------------------------
-- [in, optional] lpBackupFileName
-- The name of the file that will serve as a backup copy of the lpReplacedFileName file.
--------------------------------------------------------------------------------
-- [in] dwReplaceFlags
-- The replacement options. This parameter can be one or more of the following values.
-- 
-- REPLACEFILE_WRITE_THROUGH 0x00000001
-- This value is not supported.
-- 
-- REPLACEFILE_IGNORE_MERGE_ERRORS 0x00000002
-- Ignores errors that occur while merging information.
--
-- REPLACEFILE_IGNORE_ACL_ERRORS 0x00000004
-- Ignores errors that occur while merging ACL information.
--------------------------------------------------------------------------------
-- lpExclude
-- Reserved for future use.
--------------------------------------------------------------------------------
-- lpReserved
-- Reserved for future use.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
--
-- ERROR_UNABLE_TO_MOVE_REPLACEMENT   1176 (0x498)
-- The replacement file could not be renamed.
--
-- ERROR_UNABLE_TO_MOVE_REPLACEMENT_2 1177 (0x499)
-- The replacement file could not be moved.
--
-- ERROR_UNABLE_TO_REMOVE_REPLACED    1175 (0x497)
-- The replaced file could not be deleted.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-replacefilew
foreign import WINDOWS_CCONV unsafe "windows.h ReplaceFileW"
  c_ReplaceFile :: LPCWSTR -> LPCWSTR -> LPCWSTR -> DWORD -> LPVOID -> LPVOID -> IO Bool

----------------------------------------------------------------
-- HANDLE operations
----------------------------------------------------------------

-- | Creates or opens a file or I/O device.
--------------------------------------------------------------------------------
-- Creates or opens a file, directory, physical disk, volume, console buffer,
-- tape drive, communications resource, or other I/O device.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] lpFileName
-- The name of the file or device to be created or opened.
--------------------------------------------------------------------------------
-- [in] dwDesiredAccess
-- The requested access to the file or device (see AccessMode).
--------------------------------------------------------------------------------
-- [in] dwShareMode
-- The requested sharing mode of the file or device (see ShareMode).
--------------------------------------------------------------------------------
-- [in, optional] lpSecurityAttributes
-- A pointer to a SECURITY_ATTRIBUTES structure that determines whether the
-- returned handle can be inherited by child processes. Can be NULL.
--------------------------------------------------------------------------------
-- [in] dwCreationDisposition
-- An action to take on files that exist or do not exist (see CreateMode).
--------------------------------------------------------------------------------
-- [in] dwFlagsAndAttributes
-- The file or device attributes and flags (see FileAttributeOrFlag).
--------------------------------------------------------------------------------
-- [in, optional] hTemplateFile
-- A valid handle to a template file with the GENERIC_READ access right.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is an open handle to the specified
-- file, device, or I/O resource.
-- If the function fails, the return value is INVALID_HANDLE_VALUE.
--
-- ERROR_ALREADY_EXISTS    183   (0xB7)
-- The file already exists and cannot be created.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-createfilew
foreign import WINDOWS_CCONV unsafe "windows.h CreateFileW"
  c_CreateFile :: LPCTSTR -> AccessMode -> ShareMode -> LPSECURITY_ATTRIBUTES
               -> CreateMode -> FileAttributeOrFlag -> HANDLE -> IO HANDLE

-- | Closes an open object handle.
--------------------------------------------------------------------------------
-- Closes an open handle to an object, such as a file, process, thread, or
-- registry key.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] hObject
-- A valid handle to an open object.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
--
-- ERROR_INVALID_HANDLE    6   (0x6)
-- The handle is invalid.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/handleapi/nf-handleapi-closehandle
foreign import WINDOWS_CCONV unsafe "windows.h CloseHandle"
  c_CloseHandle :: HANDLE -> IO Bool

-- | Retrieves the file type of the specified file.
--------------------------------------------------------------------------------
-- Determines the type of a file (disk file, character file, pipe, etc.) based
-- on the specified file handle.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] hFile
-- A handle to the file whose type is to be determined.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value indicates the FileType.
-- If the function fails, the return value is FILE_TYPE_UNKNOWN.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getfiletype
foreign import WINDOWS_CCONV unsafe "windows.h GetFileType"
  getFileType :: HANDLE -> IO FileType

-- | Flushes the buffers of a specified file and causes all buffered data to be written to disk.
--------------------------------------------------------------------------------
-- Writes all buffered data for the specified file to disk. This ensures that
-- all data written to the file so far is physically stored on the disk device.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] hFile
-- A handle to the open file whose buffers are to be flushed. The handle must
-- have write access.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
--
-- ERROR_INVALID_HANDLE    6   (0x6)
-- The handle is invalid.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-flushfilebuffers
foreign import WINDOWS_CCONV unsafe "windows.h FlushFileBuffers"
  c_FlushFileBuffers :: HANDLE -> IO Bool

-- | Sets the physical end of a file.
--------------------------------------------------------------------------------
-- Moves the end-of-file (EOF) position for the specified file to the current
-- position of the file pointer. This function can be used to truncate or extend
-- a file.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] hFile
-- A handle to the file. The file handle must have GENERIC_WRITE access.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
--
-- ERROR_INVALID_HANDLE    6   (0x6)
-- The handle is invalid.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-setendoffile
foreign import WINDOWS_CCONV unsafe "windows.h SetEndOfFile"
  c_SetEndOfFile :: HANDLE -> IO Bool

-- | Sets the attributes for a file or directory.
--------------------------------------------------------------------------------
-- Sets the file system attributes for a specified file or directory.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] lpFileName
-- The name of the file or directory whose attributes are to be set.
--------------------------------------------------------------------------------
-- [in] dwFileAttributes
-- The file attributes to set for the file or directory. This parameter can be
-- a combination of FileAttributeOrFlag values.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
--
-- ERROR_INVALID_PARAMETER   87  (0x57)
-- One or more parameters are invalid.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-setfileattributesw
foreign import WINDOWS_CCONV unsafe "windows.h SetFileAttributesW"
  c_SetFileAttributes :: LPCTSTR -> FileAttributeOrFlag -> IO Bool

-- | Retrieves file system attributes for a specified file or directory.
--------------------------------------------------------------------------------
-- Retrieves attribute information for a specified file or directory.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] lpFileName
-- The name of the file or directory.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is the attributes of the file or directory.
-- If the function fails, the return value is INVALID_FILE_ATTRIBUTES.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getfileattributesw
foreign import WINDOWS_CCONV unsafe "windows.h GetFileAttributesW"
  c_GetFileAttributes :: LPCTSTR -> IO FileAttributeOrFlag

-- | Retrieves extended information about the specified file or directory.
--------------------------------------------------------------------------------
-- Retrieves attributes for a specified file or directory, including times and
-- file size, and stores them in a WIN32_FILE_ATTRIBUTE_DATA structure.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] lpFileName
-- The name of the file or directory.
--------------------------------------------------------------------------------
-- [in] fInfoLevelId
-- The information level to retrieve. Must be GetFileExInfoStandard.
--------------------------------------------------------------------------------
-- [out] lpFileInformation
-- A pointer to a buffer that receives the attribute data (typically a
-- WIN32_FILE_ATTRIBUTE_DATA structure).
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
--
-- ERROR_INVALID_PARAMETER   87  (0x57)
-- One or more parameters are invalid.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getfileattributesexw
foreign import WINDOWS_CCONV unsafe "windows.h GetFileAttributesExW"
  c_GetFileAttributesEx :: LPCTSTR -> GET_FILEEX_INFO_LEVELS -> Ptr a -> IO BOOL

-- | Retrieves file information for the specified file.
--------------------------------------------------------------------------------
-- Retrieves information about a file based on the file handle. The information
-- includes attributes, times, size, and more.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] hFile
-- A handle to the file for which information is to be retrieved.
--------------------------------------------------------------------------------
-- [out] lpFileInformation
-- A pointer to a BY_HANDLE_FILE_INFORMATION structure that receives the file
-- information.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
-- Or returns common errors.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getfileinformationbyhandle
foreign import WINDOWS_CCONV unsafe "windows.h GetFileInformationByHandle"
    c_GetFileInformationByHandle :: HANDLE -> Ptr BY_HANDLE_FILE_INFORMATION -> IO BOOL

-- | Creates a name for a temporary file and optionally creates the file.
--------------------------------------------------------------------------------
-- Generates a unique temporary file name, and optionally creates the file in
-- the specified directory.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] lpPathName
-- The directory in which the temporary file will be created.
--------------------------------------------------------------------------------
-- [in] lpPrefixString
-- The prefix of the temporary file name. The function uses the first three
-- characters of this string as the prefix.
--------------------------------------------------------------------------------
-- [in] uUnique
-- If this parameter is zero, the function creates the file. If it is nonzero,
-- the function only generates a unique file name.
--------------------------------------------------------------------------------
-- [out] lpTempFileName
-- A buffer that receives the temporary file name.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is the length, in characters, of
-- the string copied to lpTempFileName, not including the terminating null
-- character.
--
-- ERROR_BUFFER_OVERFLOW   111 (0x6F)
-- The buffer is too small to hold the generated file name.
--
-- ERROR_DIRECTORY         267 (0x10B)
-- The specified path is invalid.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-gettempfilenamew
foreign import WINDOWS_CCONV unsafe "windows.h GetTempFileNameW"
    c_GetTempFileNameW :: LPCWSTR -> LPCWSTR -> UINT -> LPWSTR -> IO UINT

----------------------------------------------------------------
-- Read/write files
----------------------------------------------------------------

-- No support for this yet
data OVERLAPPED = OVERLAPPED
  { ovl_internal     :: !ULONG_PTR -- ^ Reserved for operating system use.
  , ovl_internalHigh :: !ULONG_PTR -- ^ Reserved for operating system use.
  , ovl_offset       :: !DWORD     -- ^ Low-order part of the file position at which to start the I/O operation.
  , ovl_offsetHigh   :: !DWORD     -- ^ High-order part of the file position at which to start the I/O operation.
  , ovl_hEvent       :: !HANDLE    -- ^ Handle to an event that will be set to the signaled state when the operation has been completed.
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

-- | Reads data from a file or input/output (I/O) device.
--------------------------------------------------------------------------------
-- Reads data from the specified file or input/output device into a buffer.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] hFile
-- A handle to the file or I/O device to be read.
--------------------------------------------------------------------------------
-- [out] lpBuffer
-- A pointer to the buffer that receives the data read from the file or device.
--------------------------------------------------------------------------------
-- [in] nNumberOfBytesToRead
-- The maximum number of bytes to be read.
--------------------------------------------------------------------------------
-- [out, optional] lpNumberOfBytesRead
-- A pointer to the variable that receives the number of bytes read.
--------------------------------------------------------------------------------
-- [in, optional] lpOverlapped
-- A pointer to an OVERLAPPED structure for asynchronous operations. Can be NULL
-- for synchronous operations.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
--
-- ERROR_IO_PENDING        997 (0x3E5)
-- An overlapped I/O operation is in progress.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-readfile
foreign import WINDOWS_CCONV unsafe "windows.h ReadFile"
  c_ReadFile :: HANDLE -> Ptr a -> DWORD -> Ptr DWORD -> LPOVERLAPPED -> IO Bool

-- | Writes data to a file or input/output (I/O) device.
--------------------------------------------------------------------------------
-- Writes data from a buffer to the specified file or input/output device.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] hFile
-- A handle to the file or I/O device to be written to.
--------------------------------------------------------------------------------
-- [in] lpBuffer
-- A pointer to the buffer containing the data to be written.
--------------------------------------------------------------------------------
-- [in] nNumberOfBytesToWrite
-- The number of bytes to be written.
--------------------------------------------------------------------------------
-- [out, optional] lpNumberOfBytesWritten
-- A pointer to the variable that receives the number of bytes written.
--------------------------------------------------------------------------------
-- [in, optional] lpOverlapped
-- A pointer to an OVERLAPPED structure for asynchronous operations. Can be NULL
-- for synchronous operations.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
--
-- ERROR_IO_PENDING        997 (0x3E5)
-- An overlapped I/O operation is in progress.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-writefile
foreign import WINDOWS_CCONV unsafe "windows.h WriteFile"
  c_WriteFile :: HANDLE -> Ptr a -> DWORD -> Ptr DWORD -> LPOVERLAPPED -> IO Bool

-- | Moves the file pointer of an open file.
--------------------------------------------------------------------------------
-- Sets the file pointer for the specified file to a new position, which can be
-- specified as an offset from the beginning, current position, or end of the file.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] hFile
-- A handle to the file whose file pointer is to be moved.
--------------------------------------------------------------------------------
-- [in] liDistanceToMove
-- The number of bytes to move the file pointer.
--------------------------------------------------------------------------------
-- [out, optional] lpNewFilePointer
-- A pointer to a variable that receives the new file pointer value. Can be NULL.
--------------------------------------------------------------------------------
-- [in] dwMoveMethod
-- The starting point for the file pointer move (see FilePtrDirection).
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
--
-- ERROR_INVALID_PARAMETER 87  (0x57)
-- One or more parameters are invalid.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-setfilepointerex
foreign import WINDOWS_CCONV unsafe "windows.h SetFilePointerEx"
  c_SetFilePointerEx :: HANDLE -> LARGE_INTEGER -> Ptr LARGE_INTEGER -> FilePtrDirection -> IO Bool

----------------------------------------------------------------
-- File Notifications
--
-- Use these to initialise, "increment" and close a HANDLE you can wait
-- on.
----------------------------------------------------------------

-- | Creates a change notification handle and sets up initial change notification filter conditions.
--------------------------------------------------------------------------------
-- Creates a handle that can be used to monitor a directory or subtree for
-- changes such as file creation, deletion, or modification.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] lpPathName
-- The path of the directory to be monitored.
--------------------------------------------------------------------------------
-- [in] bWatchSubtree
-- If TRUE, the function monitors the directory tree rooted at the specified
-- directory; if FALSE, it monitors only the specified directory.
--------------------------------------------------------------------------------
-- [in] dwNotifyFilter
-- The filter conditions that satisfy a change notification wait. This parameter
-- can be one or more FileNotificationFlag values.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is a handle to the change
-- notification object. If the function fails, the return value is
-- INVALID_HANDLE_VALUE.
--
-- ERROR_INVALID_PARAMETER   87  (0x57)
-- One or more parameters are invalid.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-findfirstchangenotificationw
foreign import WINDOWS_CCONV unsafe "windows.h FindFirstChangeNotificationW"
  c_FindFirstChangeNotification :: LPCTSTR -> Bool -> FileNotificationFlag -> IO HANDLE

-- | Requests that the operating system signal a change notification handle when a change occurs.
--------------------------------------------------------------------------------
-- Continues monitoring a directory or subtree for changes after a previous
-- change notification has been received.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] hChangeHandle
-- A handle to a change notification object returned by FindFirstChangeNotificationW.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
--
-- ERROR_INVALID_HANDLE    6   (0x6)
-- The handle is invalid.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-findnextchangenotification
foreign import WINDOWS_CCONV unsafe "windows.h FindNextChangeNotification"
  c_FindNextChangeNotification :: HANDLE -> IO Bool

-- | Closes a change notification handle.
--------------------------------------------------------------------------------
-- Closes a handle created by FindFirstChangeNotificationW and releases any
-- associated resources.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] hChangeHandle
-- A handle to a change notification object returned by FindFirstChangeNotificationW.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
--
-- ERROR_INVALID_HANDLE    6   (0x6)
-- The handle is invalid.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-findclosechangenotification
foreign import WINDOWS_CCONV unsafe "windows.h FindCloseChangeNotification"
  c_FindCloseChangeNotification :: HANDLE -> IO Bool

----------------------------------------------------------------
-- Directories
----------------------------------------------------------------

type WIN32_FIND_DATA = ()

newtype FindData = FindData (ForeignPtr WIN32_FIND_DATA)

-- | Searches a directory for a file or subdirectory with a name that matches a specified pattern.
--------------------------------------------------------------------------------
-- Begins a file search in a directory, returning information about the first
-- file or subdirectory found that matches the specified pattern.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] lpFileName
-- The directory or path, and the file name, which can include wildcard
-- characters (* and ?).
--------------------------------------------------------------------------------
-- [out] lpFindFileData
-- A pointer to a WIN32_FIND_DATA structure that receives information about the
-- found file or directory.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is a search handle used in
-- subsequent calls to FindNextFile or FindClose. If the function fails, the
-- return value is INVALID_HANDLE_VALUE.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-findfirstfilew
foreign import WINDOWS_CCONV unsafe "windows.h FindFirstFileW"
  c_FindFirstFile :: LPCTSTR -> Ptr WIN32_FIND_DATA -> IO HANDLE

-- | Continues a file search from a previous call to FindFirstFileW.
--------------------------------------------------------------------------------
-- Retrieves information about the next file or directory that matches the
-- search criteria specified in a previous call to FindFirstFileW.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] hFindFile
-- The search handle returned by a previous call to FindFirstFileW.
--------------------------------------------------------------------------------
-- [out] lpFindFileData
-- A pointer to a WIN32_FIND_DATA structure that receives information about the
-- found file or directory.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero, and no more files are found
-- or an error occurred.
--
-- ERROR_NO_MORE_FILES      18  (0x12)
-- No more files were found.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-findnextfilew
foreign import WINDOWS_CCONV unsafe "windows.h FindNextFileW"
  c_FindNextFile :: HANDLE -> Ptr WIN32_FIND_DATA -> IO BOOL

-- | Closes a file search handle.
--------------------------------------------------------------------------------
-- Closes a search handle opened by FindFirstFileW or FindNextFileW and releases
-- any associated resources.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] hFindFile
-- The search handle returned by FindFirstFileW.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
--
-- ERROR_INVALID_HANDLE    6   (0x6)
-- The handle is invalid.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-findclose
foreign import WINDOWS_CCONV unsafe "windows.h FindClose"
  c_FindClose :: HANDLE -> IO BOOL

----------------------------------------------------------------
-- DOS Device flags
----------------------------------------------------------------

-- | Defines, modifies, or deletes MS-DOS device names.
--------------------------------------------------------------------------------
-- Creates, modifies, or deletes a symbolic link for a DOS device name.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] dwFlags
-- Control flags that specify the function's behavior. This parameter can be a
-- combination of DefineDosDeviceFlags values.
--------------------------------------------------------------------------------
-- [in] lpDeviceName
-- The device name string (for example, "COM1").
--------------------------------------------------------------------------------
-- [in, optional] lpTargetPath
-- The path to which the device name refers. This can be NULL when removing a
-- definition.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
--
-- ERROR_INVALID_PARAMETER   87  (0x57)
-- One or more parameters are invalid.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-definedosdevicew
foreign import WINDOWS_CCONV unsafe "windows.h DefineDosDeviceW"
  c_DefineDosDevice :: DefineDosDeviceFlags -> LPCTSTR -> LPCTSTR -> IO Bool

----------------------------------------------------------------

-- These functions are very unusual in the Win32 API:
-- They don't return error codes

-- | Determines whether the file I/O functions are using the ANSI or OEM character set.
--------------------------------------------------------------------------------
-- Indicates whether the file APIs are set to use the ANSI character set or the
-- OEM character set for file names and other string parameters.
--
-- Return value
-- If the function succeeds, the return value is nonzero if the file APIs are
-- using the ANSI character set, or zero if they are using the OEM character set.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-arefileapisansi
foreign import WINDOWS_CCONV unsafe "windows.h AreFileApisANSI"
  areFileApisANSI :: IO Bool

-- | Sets the file I/O functions to use the OEM character set.
--------------------------------------------------------------------------------
-- Configures the file APIs to use the OEM character set for file names and
-- other string parameters instead of the ANSI character set.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-setfileapistooem
foreign import WINDOWS_CCONV unsafe "windows.h SetFileApisToOEM"
  setFileApisToOEM :: IO ()

-- | Sets the file I/O functions to use the ANSI character set.
--------------------------------------------------------------------------------
-- Configures the file APIs to use the ANSI character set for file names and
-- other string parameters instead of the OEM character set.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-setfileapistoansi
foreign import WINDOWS_CCONV unsafe "windows.h SetFileApisToANSI"
  setFileApisToANSI :: IO ()

-- | Sets the maximum number of files that a process can have open simultaneously.
--------------------------------------------------------------------------------
-- Sets the size of the per-process file handle table, which determines the
-- maximum number of files that the process can have open at the same time.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] uNumber
-- The maximum number of open file handles for the process.
--------------------------------------------------------------------------------
-- Return value
-- The return value is the previous maximum number of open file handles.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-sethandlecount
foreign import WINDOWS_CCONV unsafe "windows.h SetHandleCount"
  setHandleCount :: UINT -> IO UINT

----------------------------------------------------------------

-- | Retrieves a bitmask representing the currently available disk drives.
--------------------------------------------------------------------------------
-- Returns a bitmask in which each bit represents one logical drive present on
-- the system.
--
-- Return value
-- If the function succeeds, the return value is a bitmask representing the
-- currently available disk drives. If the function fails, the return value is
-- zero.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getlogicaldrives
foreign import WINDOWS_CCONV unsafe "windows.h GetLogicalDrives"
  c_GetLogicalDrives :: IO DWORD

-- | Retrieves information about the amount of free and total space on a disk.
--------------------------------------------------------------------------------
-- Retrieves information about the specified disk, including the number of
-- sectors per cluster, bytes per sector, number of free clusters, and total
-- number of clusters.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] lpRootPathName
-- The root directory of the disk to return information about (for example,
-- "C:\").
--------------------------------------------------------------------------------
-- [out] lpSectorsPerCluster
-- A pointer to a variable that receives the number of sectors per cluster.
--------------------------------------------------------------------------------
-- [out] lpBytesPerSector
-- A pointer to a variable that receives the number of bytes per sector.
--------------------------------------------------------------------------------
-- [out] lpNumberOfFreeClusters
-- A pointer to a variable that receives the total number of free clusters on
-- the disk.
--------------------------------------------------------------------------------
-- [out] lpTotalNumberOfClusters
-- A pointer to a variable that receives the total number of clusters on the
-- disk.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
--
-- ERROR_INVALID_PARAMETER   87  (0x57)
-- One or more parameters are invalid.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getdiskfreespacew
foreign import WINDOWS_CCONV unsafe "windows.h GetDiskFreeSpaceW"
  c_GetDiskFreeSpace :: LPCTSTR -> Ptr DWORD -> Ptr DWORD -> Ptr DWORD -> Ptr DWORD -> IO Bool

-- | Sets the volume label for a specified disk volume.
--------------------------------------------------------------------------------
-- Assigns a new label to a disk volume.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] lpRootPathName
-- The root directory of the volume to be labeled (for example, "C:\").
--------------------------------------------------------------------------------
-- [in, optional] lpVolumeName
-- The new label for the volume. If this parameter is NULL, the label is deleted.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
--
-- ERROR_INVALID_PARAMETER   87  (0x57)
-- One or more parameters are invalid.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-setvolumelabelw
foreign import WINDOWS_CCONV unsafe "windows.h SetVolumeLabelW"
  c_SetVolumeLabel :: LPCTSTR -> LPCTSTR -> IO Bool

----------------------------------------------------------------
-- File locks
----------------------------------------------------------------

-- | Locks a region of a file for exclusive or shared access.
--------------------------------------------------------------------------------
-- Locks a specified region of a file, preventing access by other processes
-- depending on the requested lock mode.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] hFile
-- A handle to the file to be locked.
--------------------------------------------------------------------------------
-- [in] dwFlags
-- Specifies the lock mode. This parameter can be a combination of LockMode
-- values.
--------------------------------------------------------------------------------
-- [in] dwReserved
-- Reserved; must be zero.
--------------------------------------------------------------------------------
-- [in] nNumberOfBytesToLockLow
-- The low-order part of the length of the byte range to lock.
--------------------------------------------------------------------------------
-- [in] nNumberOfBytesToLockHigh
-- The high-order part of the length of the byte range to lock.
--------------------------------------------------------------------------------
-- [in, out] lpOverlapped
-- A pointer to an OVERLAPPED structure that specifies the starting offset of
-- the lock.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
--
-- ERROR_LOCK_VIOLATION     33  (0x21)
-- The process cannot access the file because another process has locked a
-- portion of the file.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-lockfileex
foreign import WINDOWS_CCONV unsafe "LockFileEx"
  c_LockFileEx :: HANDLE -> DWORD -> DWORD -> DWORD -> DWORD -> LPOVERLAPPED
               -> IO BOOL

-- | Unlocks a region of a file previously locked by LockFileEx.
--------------------------------------------------------------------------------
-- Unlocks a specified region of a file, allowing access by other processes.
--
-- Parameters:
--------------------------------------------------------------------------------
-- [in] hFile
-- A handle to the file to be unlocked.
--------------------------------------------------------------------------------
-- [in] dwReserved
-- Reserved; must be zero.
--------------------------------------------------------------------------------
-- [in] nNumberOfBytesToUnlockLow
-- The low-order part of the length of the byte range to unlock.
--------------------------------------------------------------------------------
-- [in] nNumberOfBytesToUnlockHigh
-- The high-order part of the length of the byte range to unlock.
--------------------------------------------------------------------------------
-- [in, out] lpOverlapped
-- A pointer to an OVERLAPPED structure that specifies the starting offset of
-- the unlock.
--------------------------------------------------------------------------------
-- Return value
-- If the function succeeds, the return value is nonzero.
-- If the function fails, the return value is zero.
--
-- ERROR_NOT_LOCKED         158 (0x9E)
-- The region specified is not locked by the calling process.
--------------------------------------------------------------------------------
-- link to original documentation:
-- https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-unlockfileex
foreign import WINDOWS_CCONV unsafe "UnlockFileEx"
  c_UnlockFileEx :: HANDLE -> DWORD -> DWORD -> DWORD -> LPOVERLAPPED -> IO BOOL

----------------------------------------------------------------
-- End
----------------------------------------------------------------
