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
module System.Win32.WindowsString.FileMapping
    ( module System.Win32.WindowsString.FileMapping
    , module System.Win32.FileMapping
    ) where

import System.Win32.FileMapping hiding
    (
      mapFile
    , withMappedFile
    , createFileMapping
    , openFileMapping
    )

import System.Win32.FileMapping.Internal
import System.Win32.WindowsString.Types   ( HANDLE, BOOL, withTString
                            , failIf, DDWORD, ddwordToDwords
                            , iNVALID_HANDLE_VALUE )
import System.Win32.Mem
import System.Win32.WindowsString.File
import System.OsString.Windows
import System.OsPath.Windows

import Control.Exception        ( mask_, bracket )
import Foreign                  ( nullPtr, maybeWith
                                , ForeignPtr, newForeignPtr )

##include "windows_cconv.h"

#include "windows.h"

---------------------------------------------------------------------------
-- Derived functions
---------------------------------------------------------------------------

-- | Maps file fully and returns ForeignPtr and length of the mapped area.
-- The mapped file is opened read-only and shared reading.
mapFile :: WindowsPath -> IO (ForeignPtr a, Int)
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
    :: WindowsPath             -- ^ Path
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

---------------------------------------------------------------------------
-- API in Haskell
---------------------------------------------------------------------------
createFileMapping :: Maybe HANDLE -> ProtectFlags -> DDWORD -> Maybe WindowsString -> IO HANDLE
createFileMapping mh flags mosize name =
    maybeWith withTString name $ \c_name ->
        failIf (==nullPtr) "createFileMapping: CreateFileMapping" $ c_CreateFileMapping handle nullPtr flags moshi moslow c_name
    where
        (moshi,moslow) = ddwordToDwords mosize
        handle = maybe iNVALID_HANDLE_VALUE id mh

openFileMapping :: FileMapAccess -> BOOL -> Maybe WindowsString -> IO HANDLE
openFileMapping access inherit name =
    maybeWith withTString name $ \c_name ->
        failIf (==nullPtr) "openFileMapping: OpenFileMapping" $
            c_OpenFileMapping access inherit c_name

