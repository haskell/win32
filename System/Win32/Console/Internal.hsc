#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Console.Internal
-- Copyright   :  (c) University of Glasgow 2023
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- Internals for Console modules.
--
-----------------------------------------------------------------------------

module System.Win32.Console.Internal where

#include <windows.h>
#include "alignment.h"
##include "windows_cconv.h"
#include "wincon_compat.h"

import System.Win32.Types
import Graphics.Win32.GDI.Types (COLORREF)

import Foreign.C.Types (CInt(..))
import Foreign.C.String (CWString)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Array (peekArray, pokeArray)

foreign import WINDOWS_CCONV unsafe "windows.h GetConsoleMode"
        c_GetConsoleMode :: HANDLE -> LPDWORD -> IO BOOL

foreign import WINDOWS_CCONV unsafe "windows.h SetConsoleMode"
        c_SetConsoleMode :: HANDLE -> DWORD -> IO BOOL

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

foreign import WINDOWS_CCONV safe "windows.h GenerateConsoleCtrlEvent"
    c_GenerateConsoleCtrlEvent :: CtrlEvent -> DWORD -> IO BOOL

foreign import WINDOWS_CCONV unsafe "Shellapi.h CommandLineToArgvW"
     c_CommandLineToArgvW :: CWString -> Ptr CInt -> IO (Ptr CWString)

foreign import WINDOWS_CCONV unsafe "processenv.h GetCommandLineW"
        getCommandLineW :: IO LPWSTR

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

data CONSOLE_SCREEN_BUFFER_INFOEX = CONSOLE_SCREEN_BUFFER_INFOEX
    { dwSizeEx              :: COORD
    , dwCursorPositionEx    :: COORD
    , wAttributesEx         :: WORD
    , srWindowEx            :: SMALL_RECT
    , dwMaximumWindowSizeEx :: COORD
    , wPopupAttributes      :: WORD
    , bFullscreenSupported  :: BOOL
    , colorTable            :: [COLORREF]
      -- ^ Only the first 16 'COLORREF' values passed to the Windows Console
      -- API. If fewer than 16 values, the remainder are padded with @0@ when
      -- passed to the API.
    } deriving (Show, Eq)

instance Storable CONSOLE_SCREEN_BUFFER_INFOEX where
    sizeOf = const #{size CONSOLE_SCREEN_BUFFER_INFOEX}
    alignment = const #{alignment CONSOLE_SCREEN_BUFFER_INFOEX}
    peek buf = do
        dwSize'               <- (#peek CONSOLE_SCREEN_BUFFER_INFOEX, dwSize) buf
        dwCursorPosition'     <- (#peek CONSOLE_SCREEN_BUFFER_INFOEX, dwCursorPosition) buf
        wAttributes'          <- (#peek CONSOLE_SCREEN_BUFFER_INFOEX, wAttributes) buf
        srWindow'             <- (#peek CONSOLE_SCREEN_BUFFER_INFOEX, srWindow) buf
        dwMaximumWindowSize'  <- (#peek CONSOLE_SCREEN_BUFFER_INFOEX, dwMaximumWindowSize) buf
        wPopupAttributes'     <- (#peek CONSOLE_SCREEN_BUFFER_INFOEX, wPopupAttributes) buf
        bFullscreenSupported' <- (#peek CONSOLE_SCREEN_BUFFER_INFOEX, bFullscreenSupported) buf
        colorTable'           <- peekArray 16 ((#ptr CONSOLE_SCREEN_BUFFER_INFOEX, ColorTable) buf)
        return $ CONSOLE_SCREEN_BUFFER_INFOEX dwSize' dwCursorPosition'
          wAttributes' srWindow' dwMaximumWindowSize' wPopupAttributes'
          bFullscreenSupported' colorTable'
    poke buf info = do
        (#poke CONSOLE_SCREEN_BUFFER_INFOEX, cbSize) buf cbSize
        (#poke CONSOLE_SCREEN_BUFFER_INFOEX, dwSize) buf (dwSizeEx info)
        (#poke CONSOLE_SCREEN_BUFFER_INFOEX, dwCursorPosition) buf (dwCursorPositionEx info)
        (#poke CONSOLE_SCREEN_BUFFER_INFOEX, wAttributes) buf (wAttributesEx info)
        (#poke CONSOLE_SCREEN_BUFFER_INFOEX, srWindow) buf (srWindowEx info)
        (#poke CONSOLE_SCREEN_BUFFER_INFOEX, dwMaximumWindowSize) buf (dwMaximumWindowSizeEx info)
        (#poke CONSOLE_SCREEN_BUFFER_INFOEX, wPopupAttributes) buf (wPopupAttributes info)
        (#poke CONSOLE_SCREEN_BUFFER_INFOEX, bFullscreenSupported) buf (bFullscreenSupported info)
        pokeArray ((#ptr CONSOLE_SCREEN_BUFFER_INFOEX, ColorTable) buf) colorTable'
      where
        cbSize :: ULONG
        cbSize = #{size CONSOLE_SCREEN_BUFFER_INFOEX}
        colorTable' = take 16 $ colorTable info ++ repeat 0

data COORD = COORD
    { xPos :: SHORT
    , yPos :: SHORT
    } deriving (Show, Eq)

instance Storable COORD where
    sizeOf = const #{size COORD}
    alignment _ = #alignment COORD
    peek buf = do
        x' <- (#peek COORD, X) buf
        y' <- (#peek COORD, Y) buf
        return $ COORD x' y'
    poke buf coord = do
        (#poke COORD, X) buf (xPos coord)
        (#poke COORD, Y) buf (yPos coord)

data SMALL_RECT = SMALL_RECT
    { leftPos   :: SHORT
    , topPos    :: SHORT
    , rightPos  :: SHORT
    , bottomPos :: SHORT
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
        (#poke SMALL_RECT, Left) buf (leftPos small_rect)
        (#poke SMALL_RECT, Top) buf (topPos small_rect)
        (#poke SMALL_RECT, Right) buf (rightPos small_rect)
        (#poke SMALL_RECT, Bottom) buf (bottomPos small_rect)

foreign import WINDOWS_CCONV safe "windows.h GetConsoleScreenBufferInfo"
    c_GetConsoleScreenBufferInfo :: HANDLE -> Ptr CONSOLE_SCREEN_BUFFER_INFO -> IO BOOL

foreign import WINDOWS_CCONV safe "windows.h GetConsoleScreenBufferInfoEx"
    c_GetConsoleScreenBufferInfoEx :: HANDLE -> Ptr CONSOLE_SCREEN_BUFFER_INFOEX -> IO BOOL

