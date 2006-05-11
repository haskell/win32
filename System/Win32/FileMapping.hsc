module System.Win32.FileMapping where

import System.Win32.Types   ( HANDLE, DWORD, BOOL, SIZE_T, LPCTSTR, withTString
                            , failIf, failIfNull, DDWORD, ddwordToDwords
                            , iNVALID_HANDLE_VALUE )
import System.Win32.Mem
import System.Win32.File
import System.Win32.Info

import Control.Exception    ( block, handle, throw, bracket )
import Data.ByteString      ( ByteString(..) )
import Foreign              ( Ptr, nullPtr, castPtr, plusPtr, maybeWith
                            , ForeignPtr )
import Foreign.Concurrent   ( newForeignPtr )

#include "windows.h"

---------------------------------------------------------------------------
-- Derived functions
---------------------------------------------------------------------------

-- | Maps file fully and returns ForeignPtr and length of the mapped area.
-- The mapped file is opened read-only and shared reading.
mapFile :: FilePath -> IO (ForeignPtr a, Int)
mapFile path = block $ do
    fh <- createFile path gENERIC_READ fILE_SHARE_READ Nothing oPEN_EXISTING fILE_ATTRIBUTE_NORMAL Nothing
    handle (\e -> closeHandle fh >> throw e) $ do
        fm <- createFileMapping (Just fh) pAGE_READONLY 0 Nothing
        handle (\e -> closeHandle fm >> throw e) $ do
            ptr <- mapViewOfFile fm fILE_MAP_READ 0 0
            fi <- getFileInformationByHandle fh
            fp <- newForeignPtr ptr (unmapViewOfFile ptr >> closeHandle fm >> closeHandle fh)
            return (fp, fromIntegral $ bhfiSize fi)

-- | As mapFile, but returns ByteString
mapFileBs :: FilePath -> IO ByteString
mapFileBs p = do
    (fp,i) <- mapFile p
    return $ PS fp 0 i

data MappedObject = MappedObject HANDLE HANDLE FileMapAccess

-- | Opens an existing file and creates mapping object to it.
withMappedFile
    :: FilePath             -- ^ Path
    -> Bool                 -- ^ Write? (False = read-only)
    -> Maybe Bool           -- ^ Sharing mode, no sharing, share read, share read+write
    -> (Integer -> MappedObject -> IO a) -- ^ Action
    -> IO a
withMappedFile path write share act = bracket make cleanup act'
    where
        act' (i,m) = act (fromIntegral i) m
        make = do
            let
                (access, page, mapaccess) = case write of
                    False   -> (gENERIC_READ, pAGE_READONLY, fILE_MAP_READ)
                    True    -> (gENERIC_READ+gENERIC_WRITE, pAGE_READWRITE, fILE_MAP_ALL_ACCESS)
                share' = case share of
                    Nothing     -> fILE_SHARE_NONE
                    Just False  -> fILE_SHARE_READ
                    Just True   -> fILE_SHARE_READ + fILE_SHARE_WRITE
            fh <- createFile path access share' Nothing oPEN_EXISTING fILE_ATTRIBUTE_NORMAL Nothing
            handle (\e -> closeHandle fh >> throw e) $ do
                bhfi <- getFileInformationByHandle fh
                fm <- createFileMapping (Just fh) page 0 Nothing
                return $ (bhfiSize bhfi, MappedObject fh fm mapaccess)
        cleanup (_,(MappedObject f m _)) = do
            closeHandle m
            closeHandle f

-- | Maps area into memory.
withMappedArea
    :: MappedObject     -- ^ Mapped object, from withMappedFile
    -> Integer          -- ^ Position in file
    -> Int              -- ^ Size of mapped area
    -> (Ptr a -> IO b)  -- ^ Action
    -> IO b
withMappedArea (MappedObject _ mh access) pos size act = do
    si <- getSystemInfo
    let gran = fromIntegral $ siAllocationGranularity si
        (blocks, offset) = divMod pos gran
        start = blocks*gran
        size' = fromIntegral $ size + fromIntegral (pos - start)
    bracket
        (mapViewOfFileEx mh access (fromIntegral start) size' nullPtr)
        (unmapViewOfFile)
        (act . flip plusPtr (fromIntegral offset))

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
-- API in Haskell
---------------------------------------------------------------------------
createFileMapping :: Maybe HANDLE -> ProtectFlags -> DDWORD -> Maybe String -> IO HANDLE
createFileMapping mh flags mosize name =
    maybeWith withTString name $ \name ->
        failIf (==nullPtr) "createFileMapping: CreateFileMapping" $ c_CreateFileMapping handle nullPtr flags moshi moslow name
    where
        (moshi,moslow) = ddwordToDwords mosize
        handle = maybe iNVALID_HANDLE_VALUE id mh

openFileMapping :: FileMapAccess -> BOOL -> Maybe String -> IO HANDLE
openFileMapping access inherit name =
    maybeWith withTString name $ \name ->
        failIf (==nullPtr) "openFileMapping: OpenFileMapping" $
            c_OpenFileMapping access inherit name

mapViewOfFileEx :: HANDLE -> FileMapAccess -> DDWORD -> SIZE_T -> Ptr a -> IO (Ptr b)
mapViewOfFileEx h access offset size base = 
    failIfNull "mapViewOfFile(Ex): c_MapViewOfFileEx" $
        c_MapViewOfFileEx h access ohi olow size base
    where
        (ohi,olow) = ddwordToDwords offset

mapViewOfFile :: HANDLE -> FileMapAccess -> DDWORD -> SIZE_T -> IO (Ptr a)
mapViewOfFile h a o s = mapViewOfFileEx h a o s nullPtr

unmapViewOfFile :: Ptr a -> IO ()
unmapViewOfFile v = c_UnmapViewOfFile v >> return ()

---------------------------------------------------------------------------
-- Imports
---------------------------------------------------------------------------
foreign import stdcall "windows.h OpenFileMappingW"
    c_OpenFileMapping :: DWORD -> BOOL -> LPCTSTR -> IO HANDLE

foreign import stdcall "windows.h CreateFileMappingW"
    c_CreateFileMapping :: HANDLE -> Ptr () -> DWORD -> DWORD -> DWORD -> LPCTSTR -> IO HANDLE 

foreign import stdcall "windows.h MapViewOfFileEx"
    c_MapViewOfFileEx :: HANDLE -> DWORD -> DWORD -> DWORD -> SIZE_T -> Ptr a -> IO (Ptr b)

foreign import stdcall "windows.h UnmapViewOfFile"
    c_UnmapViewOfFile :: Ptr a -> IO BOOL
