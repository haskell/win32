#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.FileMapping
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
module System.Win32.FileMapping
    ( mapFile
    , MappedObject(..)
    , withMappedFile
    , withMappedArea
      -- * Enums
      -- ** Section protection flags
    , ProtectSectionFlags
    , sEC_COMMIT
    , sEC_IMAGE
    , sEC_NOCACHE
    , sEC_RESERVE
      -- ** Access falgs
    , FileMapAccess
    , fILE_MAP_ALL_ACCESS
    , fILE_MAP_COPY
    , fILE_MAP_READ
    , fILE_MAP_WRITE
    , fILE_SHARE_WRITE

      -- * Mapping files
    , createFileMapping
    , openFileMapping
    , mapViewOfFileEx
    , mapViewOfFile
    , unmapViewOfFile
    ) where


import System.Win32.FileMapping.Internal
import System.Win32.Types   ( HANDLE, BOOL, SIZE_T, withTString
                            , failIf, failIfNull, DDWORD, ddwordToDwords
                            , iNVALID_HANDLE_VALUE )
import System.Win32.Mem
import System.Win32.File
import System.Win32.Info

import Control.Exception        ( mask_, bracket )
import Foreign                  ( Ptr, nullPtr, plusPtr, maybeWith
                                , ForeignPtr, newForeignPtr )

##include "windows_cconv.h"

#include "windows.h"

---------------------------------------------------------------------------
-- Derived functions
---------------------------------------------------------------------------

-- | Maps file fully and returns ForeignPtr and length of the mapped area.
-- The mapped file is opened read-only and shared reading.
mapFile :: FilePath -> IO (ForeignPtr a, Int)
mapFile path = do
    bracket
        (createFile path gENERIC_READ fILE_SHARE_READ Nothing oPEN_EXISTING fILE_ATTRIBUTE_NORMAL Nothing)
        (closeHandle)
        $ \fh -> bracket
            (createFileMapping (Just fh) pAGE_READONLY 0 Nothing)
            (closeHandle)
            $ \fm -> do
                fi <- getFileInformationByHandle fh
                fp <- mask_ $ do
                    ptr <- mapViewOfFile fm fILE_MAP_READ 0 0
                    newForeignPtr c_UnmapViewOfFileFinaliser ptr
                return (fp, fromIntegral $ bhfiSize fi)

-- | Opens an existing file and creates mapping object to it.
withMappedFile
    :: FilePath             -- ^ Path
    -> Bool                 -- ^ Write? (False = read-only)
    -> Maybe Bool           -- ^ Sharing mode, no sharing, share read, share read+write
    -> (Integer -> MappedObject -> IO a) -- ^ Action
    -> IO a
withMappedFile path write share act =
    bracket
        (createFile path access share' Nothing oPEN_EXISTING fILE_ATTRIBUTE_NORMAL Nothing)
        (closeHandle)
        $ \fh -> bracket
            (createFileMapping (Just fh) page 0 Nothing)
            (closeHandle)
            $ \fm -> do
                bhfi <- getFileInformationByHandle fh
                act (fromIntegral $ bhfiSize bhfi) (MappedObject fh fm mapaccess)
    where
        access    = if write then gENERIC_READ+gENERIC_WRITE else gENERIC_READ
        page      = if write then pAGE_READWRITE else pAGE_READONLY
        mapaccess = if write then fILE_MAP_ALL_ACCESS else fILE_MAP_READ
        share' = case share of
            Nothing     -> fILE_SHARE_NONE
            Just False  -> fILE_SHARE_READ
            Just True   -> fILE_SHARE_READ + fILE_SHARE_WRITE

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
-- API in Haskell
---------------------------------------------------------------------------
createFileMapping :: Maybe HANDLE -> ProtectFlags -> DDWORD -> Maybe String -> IO HANDLE
createFileMapping mh flags mosize name =
    maybeWith withTString name $ \c_name ->
        failIf (==nullPtr) "createFileMapping: CreateFileMapping" $ c_CreateFileMapping handle nullPtr flags moshi moslow c_name
    where
        (moshi,moslow) = ddwordToDwords mosize
        handle = maybe iNVALID_HANDLE_VALUE id mh

openFileMapping :: FileMapAccess -> BOOL -> Maybe String -> IO HANDLE
openFileMapping access inherit name =
    maybeWith withTString name $ \c_name ->
        failIf (==nullPtr) "openFileMapping: OpenFileMapping" $
            c_OpenFileMapping access inherit c_name

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

