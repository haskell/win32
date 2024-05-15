#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Trustworthy #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Console
-- Copyright   :  (c) University of Glasgow 2006
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for interfacing with Win32 Console API
--
-----------------------------------------------------------------------------

module System.Win32.Console (
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

import Data.Char (chr)
import System.Win32.Types
import System.Win32.String
import System.Win32.Console.Internal
import Graphics.Win32.Misc
import Graphics.Win32.GDI.Types (COLORREF)

import GHC.IO (bracket)
import Foreign.Ptr (plusPtr)
import Foreign.C.Types (CWchar)
import Foreign.C.String (withCWString, CWString)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Array (peekArray, peekArray0)
import Foreign.Marshal.Alloc (alloca)


getConsoleMode :: HANDLE -> IO DWORD
getConsoleMode h = alloca $ \ptr -> do
    failIfFalse_ "GetConsoleMode" $ c_GetConsoleMode h ptr
    peek ptr

setConsoleMode :: HANDLE -> DWORD -> IO ()
setConsoleMode h mode = failIfFalse_ "SetConsoleMode" $ c_SetConsoleMode h mode

eNABLE_ECHO_INPUT, eNABLE_EXTENDED_FLAGS, eNABLE_INSERT_MODE, eNABLE_LINE_INPUT,
    eNABLE_MOUSE_INPUT, eNABLE_PROCESSED_INPUT, eNABLE_QUICK_EDIT_MODE,
    eNABLE_WINDOW_INPUT, eNABLE_VIRTUAL_TERMINAL_INPUT, eNABLE_PROCESSED_OUTPUT,
    eNABLE_WRAP_AT_EOL_OUTPUT, eNABLE_VIRTUAL_TERMINAL_PROCESSING,
    dISABLE_NEWLINE_AUTO_RETURN, eNABLE_LVB_GRID_WORLDWIDE :: DWORD
eNABLE_ECHO_INPUT = 4
eNABLE_EXTENDED_FLAGS = 128
eNABLE_INSERT_MODE = 32
eNABLE_LINE_INPUT = 2
eNABLE_MOUSE_INPUT = 16
eNABLE_PROCESSED_INPUT = 1
eNABLE_QUICK_EDIT_MODE = 64
eNABLE_WINDOW_INPUT = 8
eNABLE_VIRTUAL_TERMINAL_INPUT = 512
eNABLE_PROCESSED_OUTPUT = 1
eNABLE_WRAP_AT_EOL_OUTPUT = 2
eNABLE_VIRTUAL_TERMINAL_PROCESSING = 4
dISABLE_NEWLINE_AUTO_RETURN = 8
eNABLE_LVB_GRID_WORLDWIDE = 16

generateConsoleCtrlEvent :: CtrlEvent -> DWORD -> IO ()
generateConsoleCtrlEvent e p
    = failIfFalse_
        "generateConsoleCtrlEvent"
        $ c_GenerateConsoleCtrlEvent e p

-- | This function can be used to parse command line arguments and return
--   the split up arguments as elements in a list.
commandLineToArgv :: String -> IO [String]
commandLineToArgv []  = return []
commandLineToArgv arg =
  do withCWString arg $ \c_arg -> do
       alloca $ \c_size -> do
         res <- c_CommandLineToArgvW c_arg c_size
         size <- peek c_size
         args <- peekArray (fromIntegral size) res
         _ <- localFree res
         mapM peekTString args

-- | Based on 'GetCommandLineW'. This behaves slightly different
-- than 'System.Environment.getArgs'. See the online documentation:
-- <https://learn.microsoft.com/en-us/windows/win32/api/processenv/nf-processenv-getcommandlinew>
getArgs :: IO [String]
getArgs = do
  getCommandLineW >>= peekTString >>= commandLineToArgv

getConsoleScreenBufferInfo :: HANDLE -> IO CONSOLE_SCREEN_BUFFER_INFO
getConsoleScreenBufferInfo h = alloca $ \ptr -> do
    failIfFalse_ "GetConsoleScreenBufferInfo" $ c_GetConsoleScreenBufferInfo h ptr
    peek ptr

getCurrentConsoleScreenBufferInfo :: IO CONSOLE_SCREEN_BUFFER_INFO
getCurrentConsoleScreenBufferInfo = do
    h <- failIf (== nullHANDLE) "getStdHandle" $ getStdHandle sTD_OUTPUT_HANDLE
    getConsoleScreenBufferInfo h

getConsoleScreenBufferInfoEx :: HANDLE -> IO CONSOLE_SCREEN_BUFFER_INFOEX
getConsoleScreenBufferInfoEx h = alloca $ \ptr -> do
    -- The cbSize member must be set or GetConsoleScreenBufferInfoEx fails with
    -- ERROR_INVALID_PARAMETER (87).
    (#poke CONSOLE_SCREEN_BUFFER_INFOEX, cbSize) ptr cbSize
    failIfFalse_ "GetConsoleScreenBufferInfoEx" $ c_GetConsoleScreenBufferInfoEx h ptr
    peek ptr
  where
    cbSize :: ULONG
    cbSize = #{size CONSOLE_SCREEN_BUFFER_INFOEX}

getCurrentConsoleScreenBufferInfoEx :: IO CONSOLE_SCREEN_BUFFER_INFOEX
getCurrentConsoleScreenBufferInfoEx = do
    h <- failIf (== nullHANDLE) "getStdHandle" $ getStdHandle sTD_OUTPUT_HANDLE
    getConsoleScreenBufferInfoEx h


-- c_GetEnvironmentVariableW :: LPCWSTR -> LPWSTR -> DWORD -> IO DWORD
getEnv :: String -> IO (Maybe String)
getEnv name =
  withCWString name $ \c_name -> withTStringBufferLen maxLength $ \(buf, len) -> do
    let c_len = fromIntegral len
    c_len' <- c_GetEnvironmentVariableW c_name buf c_len
    if c_len' == 0
    then do
      err_code <- getLastError
      if err_code  == eERROR_ENVVAR_NOT_FOUND
      then return Nothing
      else errorWin "GetEnvironmentVariableW"
    else do
      let len' = fromIntegral c_len'
      Just <$> peekTStringLen (buf, len')
 where
  -- according to https://learn.microsoft.com/en-us/windows/win32/api/processenv/nf-processenv-getenvironmentvariablew
  maxLength :: Int
  maxLength = 32767


getEnvironment :: IO [(String, String)]
getEnvironment = bracket c_GetEnvironmentStringsW c_FreeEnvironmentStrings $ \lpwstr -> do
    strs <- builder lpwstr
    return (divvy <$> strs)
 where
  divvy :: String -> (String, String)
  divvy str =
    case break (=='=') str of
      (xs,[])        -> (xs,[]) -- don't barf (like Posix.getEnvironment)
      (name,_:value) -> (name,value)

  builder :: LPWSTR -> IO [String]
  builder ptr = go 0
   where
    go :: Int -> IO [String]
    go off = do
      (str, l) <- peekCWStringOff ptr off
      if l == 0
      then pure []
      else (str:) <$> go (((l + 1) * 2) + off)


peekCWStringOff :: CWString -> Int -> IO (String, Int)
peekCWStringOff cp off = do
  cs <- peekArray0 wNUL (cp `plusPtr` off)
  return (cWcharsToChars cs, length cs)

wNUL :: CWchar
wNUL = 0

cWcharsToChars :: [CWchar] -> [Char]
cWcharsToChars = map chr . fromUTF16 . map fromIntegral
 where
  fromUTF16 (c1:c2:wcs)
    | 0xd800 <= c1 && c1 <= 0xdbff && 0xdc00 <= c2 && c2 <= 0xdfff =
      ((c1 - 0xd800)*0x400 + (c2 - 0xdc00) + 0x10000) : fromUTF16 wcs
  fromUTF16 (c:wcs) = c : fromUTF16 wcs
  fromUTF16 [] = []

