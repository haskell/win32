#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Info.Internal
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

module System.Win32.Info.Internal where

import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import System.Win32.Types (DWORD, LPDWORD, LPCTSTR, LPTSTR, LPVOID, UINT, WORD)

#if !MIN_VERSION_base(4,6,0)
import Prelude hiding (catch)
#endif

##include "windows_cconv.h"

#include <windows.h>
#include "alignment.h"

----------------------------------------------------------------
-- Environment Strings
----------------------------------------------------------------

-- %fun ExpandEnvironmentStrings :: String -> IO String

----------------------------------------------------------------
-- Computer Name
----------------------------------------------------------------

-- %fun GetComputerName :: IO String
-- %fun SetComputerName :: String -> IO ()
-- %end free(arg1)

----------------------------------------------------------------
-- Hardware Profiles
----------------------------------------------------------------

-- %fun GetCurrentHwProfile :: IO HW_PROFILE_INFO

----------------------------------------------------------------
-- Keyboard Type
----------------------------------------------------------------

-- %fun GetKeyboardType :: KeyboardTypeKind -> IO KeyboardType

----------------------------------------------------------------
-- System Color
----------------------------------------------------------------

type SystemColor   = UINT

-- ToDo: This list is out of date.

#{enum SystemColor,
 , cOLOR_SCROLLBAR      = COLOR_SCROLLBAR
 , cOLOR_BACKGROUND     = COLOR_BACKGROUND
 , cOLOR_ACTIVECAPTION  = COLOR_ACTIVECAPTION
 , cOLOR_INACTIVECAPTION = COLOR_INACTIVECAPTION
 , cOLOR_MENU           = COLOR_MENU
 , cOLOR_WINDOW         = COLOR_WINDOW
 , cOLOR_WINDOWFRAME    = COLOR_WINDOWFRAME
 , cOLOR_MENUTEXT       = COLOR_MENUTEXT
 , cOLOR_WINDOWTEXT     = COLOR_WINDOWTEXT
 , cOLOR_CAPTIONTEXT    = COLOR_CAPTIONTEXT
 , cOLOR_ACTIVEBORDER   = COLOR_ACTIVEBORDER
 , cOLOR_INACTIVEBORDER = COLOR_INACTIVEBORDER
 , cOLOR_APPWORKSPACE   = COLOR_APPWORKSPACE
 , cOLOR_HIGHLIGHT      = COLOR_HIGHLIGHT
 , cOLOR_HIGHLIGHTTEXT  = COLOR_HIGHLIGHTTEXT
 , cOLOR_BTNFACE        = COLOR_BTNFACE
 , cOLOR_BTNSHADOW      = COLOR_BTNSHADOW
 , cOLOR_GRAYTEXT       = COLOR_GRAYTEXT
 , cOLOR_BTNTEXT        = COLOR_BTNTEXT
 , cOLOR_INACTIVECAPTIONTEXT = COLOR_INACTIVECAPTIONTEXT
 , cOLOR_BTNHIGHLIGHT   = COLOR_BTNHIGHLIGHT
 }

-- %fun GetSysColor :: SystemColor -> IO COLORREF
-- %fun SetSysColors :: [(SystemColor,COLORREF)] -> IO ()

----------------------------------------------------------------
-- Standard Directories
----------------------------------------------------------------

foreign import WINDOWS_CCONV unsafe "GetWindowsDirectoryW"
  c_getWindowsDirectory :: LPTSTR -> UINT -> IO UINT

foreign import WINDOWS_CCONV unsafe "GetSystemDirectoryW"
  c_getSystemDirectory :: LPTSTR -> UINT -> IO UINT

foreign import WINDOWS_CCONV unsafe "GetCurrentDirectoryW"
  c_getCurrentDirectory :: DWORD -> LPTSTR -> IO UINT

foreign import WINDOWS_CCONV unsafe "GetTempPathW"
  c_getTempPath :: DWORD -> LPTSTR -> IO UINT

foreign import WINDOWS_CCONV unsafe "GetFullPathNameW"
  c_GetFullPathName :: LPCTSTR -> DWORD -> LPTSTR -> Ptr LPTSTR -> IO DWORD

foreign import WINDOWS_CCONV unsafe "GetLongPathNameW"
  c_GetLongPathName :: LPCTSTR -> LPTSTR -> DWORD -> IO DWORD

foreign import WINDOWS_CCONV unsafe "GetShortPathNameW"
  c_GetShortPathName :: LPCTSTR -> LPTSTR -> DWORD -> IO DWORD

foreign import WINDOWS_CCONV unsafe "SearchPathW"
  c_SearchPath :: LPCTSTR -> LPCTSTR -> LPCTSTR -> DWORD -> LPTSTR -> Ptr LPTSTR
               -> IO DWORD

----------------------------------------------------------------
-- System Info (Info about processor and memory subsystem)
----------------------------------------------------------------

data ProcessorArchitecture = PaUnknown WORD | PaIntel | PaMips | PaAlpha | PaPpc | PaIa64 | PaIa32OnIa64 | PaAmd64
    deriving (Show,Eq)

instance Storable ProcessorArchitecture where
    sizeOf _ = sizeOf (undefined::WORD)
    alignment _ = alignment (undefined::WORD)
    poke buf pa = pokeByteOff buf 0 $ case pa of
        PaUnknown w -> w
        PaIntel     -> #const PROCESSOR_ARCHITECTURE_INTEL
        PaMips      -> #const PROCESSOR_ARCHITECTURE_MIPS
        PaAlpha     -> #const PROCESSOR_ARCHITECTURE_ALPHA
        PaPpc       -> #const PROCESSOR_ARCHITECTURE_PPC
        PaIa64      -> #const PROCESSOR_ARCHITECTURE_IA64
#ifndef __WINE_WINDOWS_H
        PaIa32OnIa64 -> #const PROCESSOR_ARCHITECTURE_IA32_ON_WIN64
#endif
        PaAmd64     -> #const PROCESSOR_ARCHITECTURE_AMD64
    peek buf = do
        v <- (peekByteOff buf 0:: IO WORD)
        return $ case v of
            (#const PROCESSOR_ARCHITECTURE_INTEL) -> PaIntel
            (#const PROCESSOR_ARCHITECTURE_MIPS)  -> PaMips
            (#const PROCESSOR_ARCHITECTURE_ALPHA) -> PaAlpha
            (#const PROCESSOR_ARCHITECTURE_PPC)   -> PaPpc
            (#const PROCESSOR_ARCHITECTURE_IA64)  -> PaIa64
#ifndef __WINE_WINDOWS_H
            (#const PROCESSOR_ARCHITECTURE_IA32_ON_WIN64) -> PaIa32OnIa64
#endif
            (#const PROCESSOR_ARCHITECTURE_AMD64) -> PaAmd64
            w                                   -> PaUnknown w

data SYSTEM_INFO = SYSTEM_INFO
    { siProcessorArchitecture :: ProcessorArchitecture
    , siPageSize :: DWORD
    , siMinimumApplicationAddress, siMaximumApplicationAddress :: LPVOID
    , siActiveProcessorMask :: DWORD
    , siNumberOfProcessors :: DWORD
    , siProcessorType :: DWORD
    , siAllocationGranularity :: DWORD
    , siProcessorLevel :: WORD
    , siProcessorRevision :: WORD
    } deriving (Show)

instance Storable SYSTEM_INFO where
    sizeOf = const #size SYSTEM_INFO
    alignment _ = #alignment SYSTEM_INFO
    poke buf si = do
        (#poke SYSTEM_INFO, wProcessorArchitecture) buf (siProcessorArchitecture si)
        (#poke SYSTEM_INFO, dwPageSize)             buf (siPageSize si)
        (#poke SYSTEM_INFO, lpMinimumApplicationAddress) buf (siMinimumApplicationAddress si)
        (#poke SYSTEM_INFO, lpMaximumApplicationAddress) buf (siMaximumApplicationAddress si)
        (#poke SYSTEM_INFO, dwActiveProcessorMask)  buf (siActiveProcessorMask si)
        (#poke SYSTEM_INFO, dwNumberOfProcessors)   buf (siNumberOfProcessors si)
        (#poke SYSTEM_INFO, dwProcessorType)        buf (siProcessorType si)
        (#poke SYSTEM_INFO, dwAllocationGranularity) buf (siAllocationGranularity si)
        (#poke SYSTEM_INFO, wProcessorLevel)        buf (siProcessorLevel si)
        (#poke SYSTEM_INFO, wProcessorRevision)     buf (siProcessorRevision si)

    peek buf = do
        processorArchitecture <-
            (#peek SYSTEM_INFO, wProcessorArchitecture) buf
        pageSize            <- (#peek SYSTEM_INFO, dwPageSize) buf
        minimumApplicationAddress <-
            (#peek SYSTEM_INFO, lpMinimumApplicationAddress) buf
        maximumApplicationAddress <-
            (#peek SYSTEM_INFO, lpMaximumApplicationAddress) buf
        activeProcessorMask <- (#peek SYSTEM_INFO, dwActiveProcessorMask) buf
        numberOfProcessors  <- (#peek SYSTEM_INFO, dwNumberOfProcessors) buf
        processorType       <- (#peek SYSTEM_INFO, dwProcessorType) buf
        allocationGranularity <-
            (#peek SYSTEM_INFO, dwAllocationGranularity) buf
        processorLevel      <- (#peek SYSTEM_INFO, wProcessorLevel) buf
        processorRevision   <- (#peek SYSTEM_INFO, wProcessorRevision) buf
        return $ SYSTEM_INFO {
            siProcessorArchitecture     = processorArchitecture,
            siPageSize                  = pageSize,
            siMinimumApplicationAddress = minimumApplicationAddress,
            siMaximumApplicationAddress = maximumApplicationAddress,
            siActiveProcessorMask       = activeProcessorMask,
            siNumberOfProcessors        = numberOfProcessors,
            siProcessorType             = processorType,
            siAllocationGranularity     = allocationGranularity,
            siProcessorLevel            = processorLevel,
            siProcessorRevision         = processorRevision
            }

foreign import WINDOWS_CCONV unsafe "windows.h GetSystemInfo"
    c_GetSystemInfo :: Ptr SYSTEM_INFO -> IO ()

----------------------------------------------------------------
-- System metrics
----------------------------------------------------------------

type SMSetting = UINT

#{enum SMSetting,
 , sM_ARRANGE           = SM_ARRANGE
 , sM_CLEANBOOT         = SM_CLEANBOOT
 , sM_CMETRICS          = SM_CMETRICS
 , sM_CMOUSEBUTTONS     = SM_CMOUSEBUTTONS
 , sM_CXBORDER          = SM_CXBORDER
 , sM_CYBORDER          = SM_CYBORDER
 , sM_CXCURSOR          = SM_CXCURSOR
 , sM_CYCURSOR          = SM_CYCURSOR
 , sM_CXDLGFRAME        = SM_CXDLGFRAME
 , sM_CYDLGFRAME        = SM_CYDLGFRAME
 , sM_CXDOUBLECLK       = SM_CXDOUBLECLK
 , sM_CYDOUBLECLK       = SM_CYDOUBLECLK
 , sM_CXDRAG            = SM_CXDRAG
 , sM_CYDRAG            = SM_CYDRAG
 , sM_CXEDGE            = SM_CXEDGE
 , sM_CYEDGE            = SM_CYEDGE
 , sM_CXFRAME           = SM_CXFRAME
 , sM_CYFRAME           = SM_CYFRAME
 , sM_CXFULLSCREEN      = SM_CXFULLSCREEN
 , sM_CYFULLSCREEN      = SM_CYFULLSCREEN
 , sM_CXHSCROLL         = SM_CXHSCROLL
 , sM_CYVSCROLL         = SM_CYVSCROLL
 , sM_CXICON            = SM_CXICON
 , sM_CYICON            = SM_CYICON
 , sM_CXICONSPACING     = SM_CXICONSPACING
 , sM_CYICONSPACING     = SM_CYICONSPACING
 , sM_CXMAXIMIZED       = SM_CXMAXIMIZED
 , sM_CYMAXIMIZED       = SM_CYMAXIMIZED
 , sM_CXMENUCHECK       = SM_CXMENUCHECK
 , sM_CYMENUCHECK       = SM_CYMENUCHECK
 , sM_CXMENUSIZE        = SM_CXMENUSIZE
 , sM_CYMENUSIZE        = SM_CYMENUSIZE
 , sM_CXMIN             = SM_CXMIN
 , sM_CYMIN             = SM_CYMIN
 , sM_CXMINIMIZED       = SM_CXMINIMIZED
 , sM_CYMINIMIZED       = SM_CYMINIMIZED
 , sM_CXMINTRACK        = SM_CXMINTRACK
 , sM_CYMINTRACK        = SM_CYMINTRACK
 , sM_CXSCREEN          = SM_CXSCREEN
 , sM_CYSCREEN          = SM_CYSCREEN
 , sM_CXSIZE            = SM_CXSIZE
 , sM_CYSIZE            = SM_CYSIZE
 , sM_CXSIZEFRAME       = SM_CXSIZEFRAME
 , sM_CYSIZEFRAME       = SM_CYSIZEFRAME
 , sM_CXSMICON          = SM_CXSMICON
 , sM_CYSMICON          = SM_CYSMICON
 , sM_CXSMSIZE          = SM_CXSMSIZE
 , sM_CYSMSIZE          = SM_CYSMSIZE
 , sM_CXVSCROLL         = SM_CXVSCROLL
 , sM_CYHSCROLL         = SM_CYHSCROLL
 , sM_CYVTHUMB          = SM_CYVTHUMB
 , sM_CYCAPTION         = SM_CYCAPTION
 , sM_CYKANJIWINDOW     = SM_CYKANJIWINDOW
 , sM_CYMENU            = SM_CYMENU
 , sM_CYSMCAPTION       = SM_CYSMCAPTION
 , sM_DBCSENABLED       = SM_DBCSENABLED
 , sM_DEBUG             = SM_DEBUG
 , sM_MENUDROPALIGNMENT = SM_MENUDROPALIGNMENT
 , sM_MIDEASTENABLED    = SM_MIDEASTENABLED
 , sM_MOUSEPRESENT      = SM_MOUSEPRESENT
 , sM_NETWORK           = SM_NETWORK
 , sM_PENWINDOWS        = SM_PENWINDOWS
 , sM_SECURE            = SM_SECURE
 , sM_SHOWSOUNDS        = SM_SHOWSOUNDS
 , sM_SLOWMACHINE       = SM_SLOWMACHINE
 , sM_SWAPBUTTON        = SM_SWAPBUTTON
 }

-- %fun GetSystemMetrics :: SMSetting -> IO Int

----------------------------------------------------------------
-- Thread Desktops
----------------------------------------------------------------

-- %fun GetThreadDesktop :: ThreadId -> IO HDESK
-- %fun SetThreadDesktop :: ThreadId -> HDESK -> IO ()

----------------------------------------------------------------
-- User name
----------------------------------------------------------------

-- %fun GetUserName :: IO String

foreign import WINDOWS_CCONV unsafe "windows.h GetUserNameW"
  c_GetUserName :: LPTSTR -> LPDWORD -> IO Bool

----------------------------------------------------------------
-- End
----------------------------------------------------------------
