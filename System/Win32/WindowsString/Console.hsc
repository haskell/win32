{-# LANGUAGE PackageImports #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.WindowsString.Console
-- Copyright   :  (c) University of Glasgow 2023
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for interfacing with Win32 Console API (WindowsString variant)
--
-----------------------------------------------------------------------------

module System.Win32.WindowsString.Console (
        -- * Console mode
        getConsoleMode,
        setConsoleMode,
        eNABLE_ECHO_INPUT,
        eNABLE_EXTENDED_FLAGS,
        eNABLE_INSERT_MODE,
        eNABLE_LINE_INPUT,
        eNABLE_MOUSE_INPUT,
        eNABLE_PROCESSED_INPUT,
        eNABLE_QUICK_EDIT_MODE,
        eNABLE_WINDOW_INPUT,
        eNABLE_VIRTUAL_TERMINAL_INPUT,
        eNABLE_PROCESSED_OUTPUT,
        eNABLE_WRAP_AT_EOL_OUTPUT,
        eNABLE_VIRTUAL_TERMINAL_PROCESSING,
        dISABLE_NEWLINE_AUTO_RETURN,
        eNABLE_LVB_GRID_WORLDWIDE,
        -- * Console code pages
        getConsoleCP,
        setConsoleCP,
        getConsoleOutputCP,
        setConsoleOutputCP,
        -- * Ctrl events
        CtrlEvent, cTRL_C_EVENT, cTRL_BREAK_EVENT,
        generateConsoleCtrlEvent,
        -- * Command line
        commandLineToArgv,
        getCommandLineW,
        getArgs,
        -- * Screen buffer
        CONSOLE_SCREEN_BUFFER_INFO(..),
        CONSOLE_SCREEN_BUFFER_INFOEX(..),
        COORD(..),
        SMALL_RECT(..),
        COLORREF,
        getConsoleScreenBufferInfo,
        getCurrentConsoleScreenBufferInfo,
        getConsoleScreenBufferInfoEx,
        getCurrentConsoleScreenBufferInfoEx,

        -- * Env
        getEnv,
        getEnvironment
  ) where

#include <windows.h>
#include "alignment.h"
##include "windows_cconv.h"
#include "wincon_compat.h"

import System.Win32.WindowsString.Types
import System.Win32.WindowsString.String (withTStringBufferLen)
import System.Win32.Console.Internal
import System.Win32.Console hiding (getArgs, commandLineToArgv, getEnv, getEnvironment)
import System.OsString.Windows
import System.OsString.Internal.Types

import Foreign.C.Types (CWchar)
import Foreign.C.String (CWString)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Array (peekArray, peekArray0)
import Foreign.Marshal.Alloc (alloca)
import GHC.IO (bracket)
import GHC.IO.Exception (IOException(..), IOErrorType(OtherError))

import Prelude hiding (break, length, tail)
import qualified Prelude as P

#if !MIN_VERSION_filepath(1,5,0)
import Data.Coerce
import qualified "filepath" System.OsPath.Data.ByteString.Short.Word16 as BC

tail :: WindowsString -> WindowsString
tail = coerce BC.tail

break :: (WindowsChar -> Bool) -> WindowsString -> (WindowsString, WindowsString)
break = coerce BC.break
#endif


-- | This function can be used to parse command line arguments and return
--   the split up arguments as elements in a list.
commandLineToArgv :: WindowsString -> IO [WindowsString]
commandLineToArgv arg
  | arg == mempty = return []
  | otherwise = withTString arg $ \c_arg -> do
       alloca $ \c_size -> do
         res <- c_CommandLineToArgvW c_arg c_size
         size <- peek c_size
         args <- peekArray (fromIntegral size) res
         _ <- localFree res
         mapM peekTString args

-- | Based on 'GetCommandLineW'. This behaves slightly different
-- than 'System.Environment.getArgs'. See the online documentation:
-- <https://learn.microsoft.com/en-us/windows/win32/api/processenv/nf-processenv-getcommandlinew>
getArgs :: IO [WindowsString]
getArgs = do
  getCommandLineW >>= peekTString >>= commandLineToArgv


-- c_GetEnvironmentVariableW :: LPCWSTR -> LPWSTR -> DWORD -> IO DWORD
getEnv :: WindowsString -> IO (Maybe WindowsString)
getEnv name =
  withTString name $ \c_name -> withTStringBufferLen maxLength $ \(buf, len) -> do
    let c_len = fromIntegral len
    c_len' <- c_GetEnvironmentVariableW c_name buf c_len
    case c_len' of
      0 -> do
        err_code <- getLastError
        if err_code  == eERROR_ENVVAR_NOT_FOUND
        then return Nothing
        else errorWin "GetEnvironmentVariableW"
      _ | c_len' > fromIntegral maxLength ->
            -- shouldn't happen, because we provide maxLength
            ioError (IOError Nothing OtherError "GetEnvironmentVariableW" ("Unexpected return code: " <> show c_len') Nothing Nothing)
        | otherwise -> do
            let len' = fromIntegral c_len'
            Just <$> peekTStringLen (buf, len')
 where
  -- according to https://learn.microsoft.com/en-us/windows/win32/api/processenv/nf-processenv-getenvironmentvariablew
  -- max characters (wide chars): 32767
  -- => bytes = 32767 * 2 = 65534
  -- +1 byte for NUL (although not needed I think)
  maxLength :: Int
  maxLength = 65535


getEnvironment :: IO [(WindowsString, WindowsString)]
getEnvironment = bracket c_GetEnvironmentStringsW c_FreeEnvironmentStrings $ \lpwstr -> do
    strs <- builder lpwstr
    return (divvy <$> strs)
 where
  divvy :: WindowsString -> (WindowsString, WindowsString)
  divvy str =
    case break (== unsafeFromChar '=') str of
      (xs,ys)
        | ys == mempty -> (xs,ys) -- don't barf (like Posix.getEnvironment)
      (name, ys) -> let value = tail ys in (name,value)

  builder :: LPWSTR -> IO [WindowsString]
  builder ptr = go 0
   where
    go :: Int -> IO [WindowsString]
    go off = do
      (str, l) <- peekCWStringOff ptr off
      if l == 0
      then pure []
      else (str:) <$> go (((l + 1) * 2) + off)


peekCWStringOff :: CWString -> Int -> IO (WindowsString, Int)
peekCWStringOff cp off = do
  cs <- peekArray0 wNUL (cp `plusPtr` off)
  return (cWcharsToChars cs, P.length cs)

wNUL :: CWchar
wNUL = 0

cWcharsToChars :: [CWchar] -> WindowsString
cWcharsToChars = pack . fmap (WindowsChar . fromIntegral)

