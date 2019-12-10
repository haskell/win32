#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
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
        -- * Screen buffer
        CONSOLE_SCREEN_BUFFER_INFO(..),
        COORD(..),
        SMALL_RECT(..),
        getConsoleScreenBufferInfo,
        getCurrentConsoleScreenBufferInfo
  ) where

#include <windows.h>
#include "alignment.h"
##include "windows_cconv.h"

import System.Win32.Types
import Graphics.Win32.Misc

import Foreign.C.Types (CInt(..))
import Foreign.C.String (withCWString, CWString)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Alloc (alloca)

foreign import WINDOWS_CCONV unsafe "windows.h GetConsoleMode"
        c_GetConsoleMode :: HANDLE -> LPDWORD -> IO BOOL

foreign import WINDOWS_CCONV unsafe "windows.h SetConsoleMode"
        c_SetConsoleMode :: HANDLE -> DWORD -> IO BOOL

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

foreign import WINDOWS_CCONV unsafe "windows.h GetConsoleCP"
        getConsoleCP :: IO UINT

foreign import WINDOWS_CCONV unsafe "windows.h SetConsoleCP"
        setConsoleCP :: UINT -> IO ()

foreign import WINDOWS_CCONV unsafe "windows.h GetConsoleOutputCP"
        getConsoleOutputCP :: IO UINT

foreign import WINDOWS_CCONV unsafe "windows.h SetConsoleOutputCP"
        setConsoleOutputCP :: UINT -> IO ()

type CtrlEvent = DWORD
#{enum CtrlEvent,
    , cTRL_C_EVENT      = 0
    , cTRL_BREAK_EVENT  = 1
    }

generateConsoleCtrlEvent :: CtrlEvent -> DWORD -> IO ()
generateConsoleCtrlEvent e p
    = failIfFalse_
        "generateConsoleCtrlEvent"
        $ c_GenerateConsoleCtrlEvent e p

foreign import WINDOWS_CCONV safe "windows.h GenerateConsoleCtrlEvent"
    c_GenerateConsoleCtrlEvent :: CtrlEvent -> DWORD -> IO BOOL

foreign import WINDOWS_CCONV unsafe "Shellapi.h CommandLineToArgvW"
     c_CommandLineToArgvW :: CWString -> Ptr CInt -> IO (Ptr CWString)

-- | This function can be used to parse commandline arguments and return
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

data CONSOLE_SCREEN_BUFFER_INFO = CONSOLE_SCREEN_BUFFER_INFO
    { dwSize              :: COORD
    , dwCursorPosition    :: COORD
    , wAttributes         :: WORD
    , srWindow            :: SMALL_RECT
    , dwMaximumWindowSize :: COORD
    } deriving (Show, Eq)

instance Storable CONSOLE_SCREEN_BUFFER_INFO where
    sizeOf = const #{size CONSOLE_SCREEN_BUFFER_INFO}
    alignment _ = #alignment CONSOLE_SCREEN_BUFFER_INFO
    peek buf = do
        dwSize'              <- (#peek CONSOLE_SCREEN_BUFFER_INFO, dwSize) buf
        dwCursorPosition'    <- (#peek CONSOLE_SCREEN_BUFFER_INFO, dwCursorPosition) buf
        wAttributes'         <- (#peek CONSOLE_SCREEN_BUFFER_INFO, wAttributes) buf
        srWindow'            <- (#peek CONSOLE_SCREEN_BUFFER_INFO, srWindow) buf
        dwMaximumWindowSize' <- (#peek CONSOLE_SCREEN_BUFFER_INFO, dwMaximumWindowSize) buf
        return $ CONSOLE_SCREEN_BUFFER_INFO dwSize' dwCursorPosition' wAttributes' srWindow' dwMaximumWindowSize'
    poke buf info = do
        (#poke CONSOLE_SCREEN_BUFFER_INFO, dwSize) buf (dwSize info)
        (#poke CONSOLE_SCREEN_BUFFER_INFO, dwCursorPosition) buf (dwCursorPosition info)
        (#poke CONSOLE_SCREEN_BUFFER_INFO, wAttributes) buf (wAttributes info)
        (#poke CONSOLE_SCREEN_BUFFER_INFO, srWindow) buf (srWindow info)
        (#poke CONSOLE_SCREEN_BUFFER_INFO, dwMaximumWindowSize) buf (dwMaximumWindowSize info)

data COORD = COORD
    { x :: SHORT
    , y :: SHORT
    } deriving (Show, Eq)

instance Storable COORD where
    sizeOf = const #{size COORD}
    alignment _ = #alignment COORD
    peek buf = do
        x' <- (#peek COORD, X) buf
        y' <- (#peek COORD, Y) buf
        return $ COORD x' y'
    poke buf coord = do
        (#poke COORD, X) buf (x coord)
        (#poke COORD, Y) buf (y coord)

data SMALL_RECT = SMALL_RECT
    { left   :: SHORT
    , top    :: SHORT
    , right  :: SHORT
    , bottom :: SHORT
    } deriving (Show, Eq)

instance Storable SMALL_RECT where
    sizeOf _ = #{size SMALL_RECT}
    alignment _ = #alignment SMALL_RECT
    peek buf = do
        left'   <- (#peek SMALL_RECT, Left) buf
        top'    <- (#peek SMALL_RECT, Top) buf
        right'  <- (#peek SMALL_RECT, Right) buf
        bottom' <- (#peek SMALL_RECT, Bottom) buf
        return $ SMALL_RECT left' top' right' bottom'
    poke buf small_rect = do
        (#poke SMALL_RECT, Left) buf (left small_rect)
        (#poke SMALL_RECT, Top) buf (top small_rect)
        (#poke SMALL_RECT, Right) buf (right small_rect)
        (#poke SMALL_RECT, Bottom) buf (bottom small_rect)

foreign import WINDOWS_CCONV safe "windows.h GetConsoleScreenBufferInfo"
    c_GetConsoleScreenBufferInfo :: HANDLE -> Ptr CONSOLE_SCREEN_BUFFER_INFO -> IO BOOL

getConsoleScreenBufferInfo :: HANDLE -> IO CONSOLE_SCREEN_BUFFER_INFO
getConsoleScreenBufferInfo h = alloca $ \ptr -> do
    failIfFalse_ "GetConsoleScreenBufferInfo" $ c_GetConsoleScreenBufferInfo h ptr
    peek ptr

getCurrentConsoleScreenBufferInfo :: IO CONSOLE_SCREEN_BUFFER_INFO
getCurrentConsoleScreenBufferInfo = do
    h <- failIf (== nullHANDLE) "getStdHandle" $ getStdHandle sTD_OUTPUT_HANDLE
    getConsoleScreenBufferInfo h
