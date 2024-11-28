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

import Foreign.C.Types (CInt(..), CWchar)
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

foreign import WINDOWS_CCONV unsafe "processenv.h GetEnvironmentVariableW"
        c_GetEnvironmentVariableW :: LPCWSTR -> LPWSTR -> DWORD -> IO DWORD

foreign import WINDOWS_CCONV unsafe "processenv.h GetEnvironmentStringsW"
        c_GetEnvironmentStringsW :: IO LPWSTR

foreign import WINDOWS_CCONV unsafe "processenv.h FreeEnvironmentStringsW"
  c_FreeEnvironmentStrings :: LPWSTR -> IO Bool

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

-- | This type represents a keyboard input event. The structure is documented here:
-- https://learn.microsoft.com/en-us/windows/console/key-event-record-str
data KEY_EVENT_RECORD = KEY_EVENT_RECORD
    { keyDown          :: BOOL
    , repeatCount      :: WORD
    , virtualKeyCode   :: WORD
    , virtualScanCode  :: WORD
    , uChar            :: CWchar
    , controlKeyStateK :: DWORD
    } deriving (Eq, Show)

-- | This type represents a mouse event. The structure is documented here:
-- https://learn.microsoft.com/en-us/windows/console/mouse-event-record-str
data MOUSE_EVENT_RECORD = MOUSE_EVENT_RECORD
  { mousePosition    :: COORD
  , buttonState      :: DWORD
  , controlKeyStateM :: DWORD
  , eventFlags       :: DWORD
  } deriving (Eq, Show)

-- | This type represents a window size change event. The structure is documented here:
-- https://learn.microsoft.com/en-us/windows/console/window-buffer-size-record-str
newtype WINDOW_BUFFER_SIZE_RECORD = WINDOW_BUFFER_SIZE_RECORD
  { windowSize :: COORD
  } deriving (Eq, Show)

-- | This type represents a window menu event. (Current ignored by VTY). The structure
-- is documented here: https://learn.microsoft.com/en-us/windows/console/menu-event-record-str
newtype MENU_EVENT_RECORD = MENU_EVENT_RECORD
  { commandId :: UINT
  } deriving (Eq, Show)

-- | This type represents a window focus change event. The structure is documented here:
-- https://learn.microsoft.com/en-us/windows/console/focus-event-record-str
newtype FOCUS_EVENT_RECORD = FOCUS_EVENT_RECORD
  { setFocus :: BOOL
  } deriving (Eq, Show)

-- | Description of a Windows console input event. Documented here:
-- https://learn.microsoft.com/en-us/windows/console/input-record-str
data INPUT_RECORD =
    KeyEvent KEY_EVENT_RECORD
  | MouseEvent MOUSE_EVENT_RECORD
  | WindowBufferSizeEvent WINDOW_BUFFER_SIZE_RECORD
  | MenuEvent MENU_EVENT_RECORD
  | FocusEvent FOCUS_EVENT_RECORD
  deriving (Eq, Show)

instance Storable KEY_EVENT_RECORD where
    sizeOf = const #{size KEY_EVENT_RECORD}
    alignment _ = #alignment KEY_EVENT_RECORD
    poke buf input = do
        (#poke KEY_EVENT_RECORD, bKeyDown)          buf (keyDown input)
        (#poke KEY_EVENT_RECORD, wRepeatCount)      buf (repeatCount input)
        (#poke KEY_EVENT_RECORD, wVirtualKeyCode)   buf (virtualKeyCode input)
        (#poke KEY_EVENT_RECORD, wVirtualScanCode)  buf (virtualScanCode input)
        (#poke KEY_EVENT_RECORD, uChar)             buf (uChar input)
        (#poke KEY_EVENT_RECORD, dwControlKeyState) buf (controlKeyStateK input)
    peek buf = do
        keyDown'          <- (#peek KEY_EVENT_RECORD, bKeyDown) buf
        repeatCount'      <- (#peek KEY_EVENT_RECORD, wRepeatCount) buf
        virtualKeyCode'   <- (#peek KEY_EVENT_RECORD, wVirtualKeyCode) buf
        virtualScanCode'  <- (#peek KEY_EVENT_RECORD, wVirtualScanCode) buf
        uChar'            <- (#peek KEY_EVENT_RECORD, uChar) buf
        controlKeyStateK' <- (#peek KEY_EVENT_RECORD, dwControlKeyState) buf
        return $ KEY_EVENT_RECORD keyDown' repeatCount' virtualKeyCode' virtualScanCode' uChar' controlKeyStateK'

instance Storable MOUSE_EVENT_RECORD where
    sizeOf = const #{size MOUSE_EVENT_RECORD}
    alignment _ = #alignment MOUSE_EVENT_RECORD
    poke buf input = do
        (#poke MOUSE_EVENT_RECORD, dwMousePosition)   buf (mousePosition input)
        (#poke MOUSE_EVENT_RECORD, dwButtonState)     buf (buttonState input)
        (#poke MOUSE_EVENT_RECORD, dwControlKeyState) buf (controlKeyStateM input)
        (#poke MOUSE_EVENT_RECORD, dwEventFlags)      buf (eventFlags input)
    peek buf = do
        mousePosition'    <- (#peek MOUSE_EVENT_RECORD, dwMousePosition) buf
        buttonState'      <- (#peek MOUSE_EVENT_RECORD, dwButtonState) buf
        controlKeyStateM' <- (#peek MOUSE_EVENT_RECORD, dwControlKeyState) buf
        eventFlags'       <- (#peek MOUSE_EVENT_RECORD, dwEventFlags) buf
        return $ MOUSE_EVENT_RECORD mousePosition' buttonState' controlKeyStateM' eventFlags'

instance Storable WINDOW_BUFFER_SIZE_RECORD where
    sizeOf = const #{size WINDOW_BUFFER_SIZE_RECORD}
    alignment _ = #alignment WINDOW_BUFFER_SIZE_RECORD
    poke buf input = do
        (#poke WINDOW_BUFFER_SIZE_RECORD, dwSize) buf (windowSize input)
    peek buf = do
        size' <- (#peek WINDOW_BUFFER_SIZE_RECORD, dwSize) buf
        return $ WINDOW_BUFFER_SIZE_RECORD size'

instance Storable MENU_EVENT_RECORD where
    sizeOf = const #{size MENU_EVENT_RECORD}
    alignment _ = #alignment MENU_EVENT_RECORD
    poke buf input = do
        (#poke MENU_EVENT_RECORD, dwCommandId) buf (commandId input)
    peek buf = do
        commandId' <- (#peek MENU_EVENT_RECORD, dwCommandId) buf
        return $ MENU_EVENT_RECORD commandId'

instance Storable FOCUS_EVENT_RECORD where
    sizeOf = const #{size FOCUS_EVENT_RECORD}
    alignment _ = #alignment FOCUS_EVENT_RECORD
    poke buf input = do
        (#poke FOCUS_EVENT_RECORD, bSetFocus) buf (setFocus input)
    peek buf = do
        setFocus' <- (#peek FOCUS_EVENT_RECORD, bSetFocus) buf
        return $ FOCUS_EVENT_RECORD setFocus'

instance Storable INPUT_RECORD where
    sizeOf = const #{size INPUT_RECORD}
    alignment _ = #alignment INPUT_RECORD

    poke buf (KeyEvent key) = do
        (#poke INPUT_RECORD, EventType) buf (#{const KEY_EVENT} :: WORD)
        (#poke INPUT_RECORD, Event) buf key
    poke buf (MouseEvent mouse) = do
        (#poke INPUT_RECORD, EventType) buf (#{const MOUSE_EVENT} :: WORD)
        (#poke INPUT_RECORD, Event) buf mouse
    poke buf (WindowBufferSizeEvent window) = do
        (#poke INPUT_RECORD, EventType) buf (#{const WINDOW_BUFFER_SIZE_EVENT} :: WORD)
        (#poke INPUT_RECORD, Event) buf window
    poke buf (MenuEvent menu) = do
        (#poke INPUT_RECORD, EventType) buf (#{const MENU_EVENT} :: WORD)
        (#poke INPUT_RECORD, Event) buf menu
    poke buf (FocusEvent focus) = do
        (#poke INPUT_RECORD, EventType) buf (#{const FOCUS_EVENT} :: WORD)
        (#poke INPUT_RECORD, Event) buf focus

    peek buf = do
        event <- (#peek INPUT_RECORD, EventType) buf :: IO WORD
        case event of
          #{const KEY_EVENT} ->
              KeyEvent `fmap` (#peek INPUT_RECORD, Event) buf
          #{const MOUSE_EVENT} ->
              MouseEvent `fmap` (#peek INPUT_RECORD, Event) buf
          #{const WINDOW_BUFFER_SIZE_EVENT} ->
              WindowBufferSizeEvent `fmap` (#peek INPUT_RECORD, Event) buf
          #{const MENU_EVENT} ->
              MenuEvent `fmap` (#peek INPUT_RECORD, Event) buf
          #{const FOCUS_EVENT} ->
              FocusEvent `fmap` (#peek INPUT_RECORD, Event) buf
          _ -> error $ "Unknown input event type " ++ show event

foreign import WINDOWS_CCONV unsafe "windows.h ReadConsoleInputW"
    c_ReadConsoleInput :: HANDLE -> Ptr INPUT_RECORD -> DWORD -> LPDWORD -> IO BOOL
