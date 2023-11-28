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
        getCurrentConsoleScreenBufferInfoEx
  ) where

#include <windows.h>
#include "alignment.h"
##include "windows_cconv.h"
#include "wincon_compat.h"

import System.Win32.WindowsString.Types
import System.Win32.Console.Internal
import System.Win32.Console hiding (getArgs, commandLineToArgv)
import System.OsString.Windows

import Foreign.Storable (Storable(..))
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Alloc (alloca)


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

