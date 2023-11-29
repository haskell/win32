{-# LANGUAGE PackageImports #-}

{- |
   Module      :  System.Win32.String
   Copyright   :  2013 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Utilities for primitive marshalling of Windows' C strings.
-}
module System.Win32.WindowsString.String
  ( LPSTR, LPCSTR, LPWSTR, LPCWSTR
  , TCHAR, LPTSTR, LPCTSTR, LPCTSTR_
  , withTString, withTStringLen, peekTString, peekTStringLen
  , newTString
  , withTStringBuffer, withTStringBufferLen
  ) where

import System.Win32.String hiding
  ( withTStringBuffer
  , withTStringBufferLen
  , withTString
  , withTStringLen
  , peekTString
  , peekTStringLen
  , newTString
  )
import System.Win32.WindowsString.Types
import System.OsString.Internal.Types
#if MIN_VERSION_filepath(1, 5, 0)
import qualified "os-string" System.OsString.Data.ByteString.Short as SBS
#else
import qualified "filepath" System.OsPath.Data.ByteString.Short as SBS
#endif
import Data.Word (Word8)

-- | Marshal a dummy Haskell string into a NUL terminated C wide string
-- using temporary storage.
--
-- * the Haskell string is created by length parameter. And the Haskell
--   string contains /only/ NUL characters.
--
-- * the memory is freed when the subcomputation terminates (either
--   normally or via an exception), so the pointer to the temporary
--   storage must /not/ be used after this.
--
withTStringBuffer :: Int -> (LPTSTR -> IO a) -> IO a
withTStringBuffer maxLength
  = let dummyBuffer = WindowsString $ SBS.pack $ replicate (if even maxLength then maxLength else maxLength + 1) _nul
    in  withTString dummyBuffer

-- | Marshal a dummy Haskell string into a C wide string (i.e. wide
-- character array) in temporary storage, with explicit length
-- information.
--
-- * the Haskell string is created by length parameter. And the Haskell
--   string contains /only/ NUL characters.
--
-- * the memory is freed when the subcomputation terminates (either
--   normally or via an exception), so the pointer to the temporary
--   storage must /not/ be used after this.
--
withTStringBufferLen :: Int -> ((LPTSTR, Int) -> IO a) -> IO a
withTStringBufferLen maxLength
  = let dummyBuffer = WindowsString $ SBS.pack $ replicate (if even maxLength then maxLength else maxLength + 1) _nul
    in  withTStringLen dummyBuffer


_nul :: Word8
_nul = 0x00
