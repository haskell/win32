-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Info
-- Copyright   :  (c) Alastair Reid, 1997-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for interfacing with Win32.
--
-----------------------------------------------------------------------------

module System.Win32.Info where

import System.Win32.Types

#include <windows.h>


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

-- %fun GetSystemDirectory  :: IO String
-- %fun GetWindowsDirectory :: IO String

----------------------------------------------------------------
-- System Info (Info about processor and memory subsystem)
----------------------------------------------------------------

-- %fun GetSystemInfo :: IO SystemInfo
--
-- typedef struct _SYSTEM_INFO { // sinf
--     union {
-- 	   DWORD  dwOemId;
-- 	   struct {
-- 	       WORD wProcessorArchitecture;
-- 	       WORD wReserved;
-- 	   };
--     };
--     DWORD  dwPageSize;
--     LPVOID lpMinimumApplicationAddress;
--     LPVOID lpMaximumApplicationAddress;
--     DWORD  dwActiveProcessorMask;
--     DWORD  dwNumberOfProcessors;
--     DWORD  dwProcessorType;
--     DWORD  dwAllocationGranularity;
--     WORD  wProcessorLevel;
--     WORD  wProcessorRevision;
-- } SYSTEM_INFO;


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

----------------------------------------------------------------
-- Version Info
----------------------------------------------------------------

-- %fun GetVersionEx :: IO VersionInfo
--
-- typedef struct _OSVERSIONINFO{
--     DWORD dwOSVersionInfoSize;
--     DWORD dwMajorVersion;
--     DWORD dwMinorVersion;
--     DWORD dwBuildNumber;
--     DWORD dwPlatformId;
--     TCHAR szCSDVersion[ 128 ];
-- } OSVERSIONINFO;

----------------------------------------------------------------
-- Processor features
----------------------------------------------------------------

--
-- Including these lines causes problems on Win95
-- %fun IsProcessorFeaturePresent :: ProcessorFeature -> Bool
--
-- type ProcessorFeature   = DWORD
-- %dis processorFeature x = dWORD x
--
-- %const ProcessorFeature
-- % [ PF_FLOATING_POINT_PRECISION_ERRATA
-- % , PF_FLOATING_POINT_EMULATED
-- % , PF_COMPARE_EXCHANGE_DOUBLE
-- % , PF_MMX_INSTRUCTIONS_AVAILABLE
-- % ]

----------------------------------------------------------------
-- System Parameter Information
----------------------------------------------------------------

-- %fun SystemParametersInfo :: ?? -> Bool -> IO ??

----------------------------------------------------------------
-- End
----------------------------------------------------------------
