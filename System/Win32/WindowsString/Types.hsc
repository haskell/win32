{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Types
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

module System.Win32.WindowsString.Types
        ( module System.Win32.WindowsString.Types
        , module System.Win32.Types
        ) where

import System.Win32.Types hiding (
    withTString
  , withTStringLen
  , peekTString
  , peekTStringLen
  , newTString
  , failIf
  , failIf_
  , failIfNeg
  , failIfNull
  , failIfZero
  , failIfFalse_
  , failUnlessSuccess
  , failUnlessSuccessOr
  , errorWin
  , failWith
  , try
  , withFilePath
  , useAsCWStringSafe
  )

import Foreign.C.Types (CUIntPtr(..))
import Foreign.C.String (CWString)
import qualified System.OsPath.Windows as WS
import System.OsPath.Windows (WindowsPath)
import System.OsString.Windows (decodeWith, encodeWith)
import System.OsString.Internal.Types
#if MIN_VERSION_filepath(1,5,0)
import "os-string" System.OsString.Encoding.Internal (decodeWithBaseWindows)
import qualified "os-string" System.OsString.Data.ByteString.Short.Word16 as SBS
import "os-string" System.OsString.Data.ByteString.Short.Word16 (
#else
import "filepath" System.OsPath.Encoding.Internal (decodeWithBaseWindows)
import qualified "filepath" System.OsPath.Data.ByteString.Short.Word16 as SBS
import "filepath" System.OsPath.Data.ByteString.Short.Word16 (
#endif
  packCWString,
  packCWStringLen,
  useAsCWString,
  useAsCWStringLen,
  newCWString
  )
import Data.Bifunctor (first)
import Data.Char (isSpace)
import Numeric (showHex)
import qualified System.IO as IO ()
import System.IO.Error (ioeSetErrorString)
import Foreign (allocaArray)
import Foreign.Ptr ( Ptr )
import Foreign.C.Error ( errnoToIOError )
import Control.Exception ( throwIO )
import qualified Control.Exception as EX
import GHC.Ptr (castPtr)
import GHC.IO.Exception

#if !MIN_VERSION_base(4,8,0)
import Data.Word (Word)
#endif

import GHC.IO.Encoding.UTF16 ( mkUTF16le )
import GHC.IO.Encoding.Failure ( CodingFailureMode(..) )

#include <fcntl.h>
#include <windows.h>
#include <wchar.h>
##include "windows_cconv.h"


----------------------------------------------------------------
-- Chars and strings
----------------------------------------------------------------

withTString    :: WindowsString -> (LPTSTR -> IO a) -> IO a
withFilePath   :: WindowsPath -> (LPTSTR -> IO a) -> IO a
withTStringLen :: WindowsString -> ((LPTSTR, Int) -> IO a) -> IO a
peekTString    :: LPCTSTR -> IO WindowsString
peekTStringLen :: (LPCTSTR, Int) -> IO WindowsString
newTString     :: WindowsString -> IO LPCTSTR

-- UTF-16 version:
-- the casts are from 'Ptr Word16' to 'Ptr CWchar', which is safe
withTString (WindowsString str) f    = useAsCWString str (\ptr -> f (castPtr ptr))
withFilePath path = useAsCWStringSafe path
withTStringLen (WindowsString str) f = useAsCWStringLen str (\(ptr, len) -> f (castPtr ptr, len))
peekTString    = fmap WindowsString . packCWString . castPtr
peekTStringLen = fmap WindowsString . packCWStringLen . first castPtr
newTString (WindowsString str) = fmap castPtr $ newCWString str

foreign import ccall unsafe "wchar.h wcslen" c_wcslen
    :: CWString -> IO SIZE_T

-- | Wrapper around 'useAsCString', checking the encoded 'FilePath' for internal NUL codepoints as these are
-- disallowed in Windows filepaths. See https://gitlab.haskell.org/ghc/ghc/-/issues/13660
useAsCWStringSafe :: WindowsPath -> (CWString -> IO a) -> IO a
useAsCWStringSafe wp@(WS path) f = useAsCWString path $ \(castPtr -> ptr) -> do
    let len = SBS.numWord16 path
    clen <- c_wcslen ptr
    if clen == fromIntegral len
        then f ptr
        else do
          path' <- either (const (_toStr wp)) id <$> (EX.try @IOException) (decodeWithBaseWindows path)
          ioError (err path')
  where
    _toStr = fmap WS.toChar . WS.unpack
    err path' =
        IOError
          { ioe_handle = Nothing
          , ioe_type = InvalidArgument
          , ioe_location = "useAsCWStringSafe"
          , ioe_description = "Windows filepaths must not contain internal NUL codepoints."
          , ioe_errno = Nothing
          , ioe_filename = Just path'
          }

----------------------------------------------------------------
-- Errors
----------------------------------------------------------------

failIf :: (a -> Bool) -> String -> IO a -> IO a
failIf p wh act = do
  v <- act
  if p v then errorWin wh else return v

failIf_ :: (a -> Bool) -> String -> IO a -> IO ()
failIf_ p wh act = do
  v <- act
  if p v then errorWin wh else return ()

failIfNeg :: (Num a, Ord a) => String -> IO a -> IO a
failIfNeg = failIf (< 0)

failIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
failIfNull = failIf (== nullPtr)

failIfZero :: (Eq a, Num a) => String -> IO a -> IO a
failIfZero = failIf (== 0)

failIfFalse_ :: String -> IO Bool -> IO ()
failIfFalse_ = failIf_ not

failUnlessSuccess :: String -> IO ErrCode -> IO ()
failUnlessSuccess fn_name act = do
  r <- act
  if r == 0 then return () else failWith fn_name r

failUnlessSuccessOr :: ErrCode -> String -> IO ErrCode -> IO Bool
failUnlessSuccessOr val fn_name act = do
  r <- act
  if r == 0 then return False
    else if r == val then return True
    else failWith fn_name r


errorWin :: String -> IO a
errorWin fn_name = do
  err_code <- getLastError
  failWith fn_name err_code

failWith :: String -> ErrCode -> IO a
failWith fn_name err_code = do
  c_msg <- getErrorMessage err_code

  msg <- either (fail . show) pure . decodeWith (mkUTF16le TransliterateCodingFailure) =<< if c_msg == nullPtr
           then either (fail . show) pure . encodeWith (mkUTF16le TransliterateCodingFailure) $ "Error 0x" ++ Numeric.showHex err_code ""
           else do msg <- peekTString c_msg
                   -- We ignore failure of freeing c_msg, given we're already failing
                   _ <- localFree c_msg
                   return msg
  -- turn GetLastError() into errno, which errnoToIOError knows how to convert
  -- to an IOException we can throw.
  errno <- c_maperrno_func err_code
  let msg' = reverse $ dropWhile isSpace $ reverse msg -- drop trailing \n
      ioerror = errnoToIOError fn_name errno Nothing Nothing
                  `ioeSetErrorString` msg'
  throwIO ioerror


-- Support for API calls that are passed a fixed-size buffer and tell
-- you via the return value if the buffer was too small.  In that
-- case, we double the buffer size and try again.
try :: String -> (LPTSTR -> UINT -> IO UINT) -> UINT -> IO WindowsString
try loc f n = do
   e <- allocaArray (fromIntegral n) $ \lptstr -> do
          r <- failIfZero loc $ f lptstr n
          if (r > n) then return (Left r) else do
            str <- peekTStringLen (lptstr, fromIntegral r)
            return (Right str)
   case e of
        Left n'   -> try loc f n'
        Right str -> return str
