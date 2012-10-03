#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Error
-- Copyright   :  (c) Alastair Reid, 1997-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Bryan O'Sullivan <bos@serpentine.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for interfacing with Win32.
--
-----------------------------------------------------------------------------

module System.Win32.Error
        ( module System.Win32.Error
        ) where

import System.Win32.Internal (getCurrentCodePage)
import System.Win32.Types

import Foreign hiding ( unsafePerformIO, void )
import Foreign.C
import Control.Concurrent ( threadDelay )
import Control.Exception
import System.IO.Error
import Control.Monad      ( void )
import Data.Char
import Numeric (showHex)

##include "windows_cconv.h"

#include <windows.h>

----------------------------------------------------------------
-- Errors
----------------------------------------------------------------

type ErrCode = DWORD

failIf :: (a -> Bool) -> String -> IO a -> IO a
failIf p wh act = do
  v <- act
  if p v then errorWin wh else return v

failIf_ :: (a -> Bool) -> String -> IO a -> IO ()
failIf_ p wh act = do
  v <- act
  if p v then errorWin wh else return ()

failIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
failIfNull = failIf (== nullPtr)

failIfZero :: (Eq a, Num a) => String -> IO a -> IO a
failIfZero = failIf (== 0)

failIfFalse_ :: String -> IO Bool -> IO ()
failIfFalse_ = failIf_ not

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
  msg <- if c_msg == nullPtr
           then return $ "Error 0x" ++ Numeric.showHex err_code ""
           else do msg <- peekTString c_msg
                   -- We ignore failure of freeing c_msg, given we're already failing
                   _ <- localFree c_msg
                   return msg
  c_maperrno -- turn GetLastError() into errno, which errnoToIOError knows
             -- how to convert to an IOException we can throw.
             -- XXX we should really do this directly.
  errno <- getErrno
  let msg' = reverse $ dropWhile isSpace $ reverse msg -- drop trailing \n
  -- Temporary fix to support multibyte error message. 
  -- Because localeEncoding doesn't support multibyte encoding on Windows (except UTF-*). #3977
  cp <- getCurrentCodePage
  msg'' <- encodeMultiByteIO cp msg' -- convert to multibytes message
  let ioerror = errnoToIOError fn_name errno Nothing Nothing
                  `ioeSetErrorString` msg''
  throw ioerror


foreign import ccall unsafe "maperrno" -- in base/cbits/Win32Utils.c
   c_maperrno :: IO ()

-- ----------------------------------------------------------------------------

-- | The `System.IO` output functions (e.g. `putStr`) don't
-- automatically convert to multibyte string on Windows, so this
-- function is provided to make the conversion from a Unicode string
-- in the given code page to a proper multibyte string.  To get the
-- code page for the console, use `getCurrentCodePage`.
--
encodeMultiByteIO :: CodePage -> String -> IO String
encodeMultiByteIO _ "" = return ""
  -- WideCharToMultiByte doesn't handle empty strings
encodeMultiByteIO cp wstr =
  withCWStringLen wstr $ \(cwstr,len) -> do
    mbchars <- failIfZero "WideCharToMultiByte" $ wideCharToMultiByte 
                cp
                0
                cwstr
                (fromIntegral len)
                nullPtr 0
                nullPtr nullPtr
    -- mbchars is the length of buffer required
    allocaArray (fromIntegral mbchars) $ \mbstr -> do
      mbchars <- failIfZero "WideCharToMultiByte" $ wideCharToMultiByte 
                 cp
                 0
                 cwstr
                 (fromIntegral len)
                 mbstr mbchars
                 nullPtr nullPtr
      peekCAStringLen (mbstr,fromIntegral mbchars)  -- converts [Char] to UTF-16

----------------------------------------------------------------
-- Primitives
----------------------------------------------------------------

foreign import WINDOWS_CCONV unsafe "windows.h LocalFree"
  localFree :: Ptr a -> IO (Ptr a)

foreign import WINDOWS_CCONV unsafe "windows.h GetLastError"
  getLastError :: IO ErrCode

{-# CFILES cbits/errors.c #-}

foreign import ccall unsafe "errors.h"
  getErrorMessage :: DWORD -> IO LPWSTR

foreign import WINDOWS_CCONV "WideCharToMultiByte"
  wideCharToMultiByte
        :: CodePage
        -> DWORD   -- dwFlags,
        -> LPCWSTR -- lpWideCharStr
        -> CInt    -- cchWideChar
        -> LPSTR   -- lpMultiByteStr
        -> CInt    -- cbMultiByte
        -> LPCSTR  -- lpMultiByteStr
        -> LPBOOL  -- lpbFlags
        -> IO CInt

----------------------------------------------------------------
-- End
----------------------------------------------------------------
