-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Win32.Misc
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

module Graphics.Win32.Misc where

import Graphics.Win32.GDI.Types
import System.Win32.Types

import Foreign

#include <windows.h>
#include "gettime.h"

----------------------------------------------------------------
-- Resources
-- (should probably be distributed between
--  Graphics.Win32.{Icon,Cursor,Accelerator,Menu,...})
----------------------------------------------------------------

type Accelerator = LPCSTR
-- intToAccelerator :: Int -> Accelerator
-- intToAccelerator i = makeIntResource (toWord i)

-- cursor and icon should not be const pointer; GSL ???
type Cursor = LPSTR
-- intToCursor :: Int -> Cursor
-- intToCursor i = makeIntResource (toWord i)

type Icon = LPSTR
-- intToIcon :: Int -> Icon
-- intToIcon i = makeIntResource (toWord i)

loadAccelerators :: Maybe HINSTANCE -> Accelerator -> IO HACCEL
loadAccelerators mb_inst accel =
  failIfNull "LoadAccelerators" $ c_LoadAccelerators (maybePtr mb_inst) accel
foreign import stdcall unsafe "windows.h LoadAcceleratorsW"
  c_LoadAccelerators :: HINSTANCE -> Accelerator -> IO HACCEL

loadCursor :: Maybe HINSTANCE -> Cursor -> IO HCURSOR
loadCursor mb_inst cursor =
  failIfNull "LoadCursor" $ c_LoadCursor (maybePtr mb_inst) cursor
foreign import stdcall unsafe "windows.h LoadCursorW"
  c_LoadCursor :: HINSTANCE -> Cursor -> IO HCURSOR

loadIcon :: Maybe HINSTANCE -> Icon -> IO HICON
loadIcon mb_inst icon =
  failIfNull "LoadIcon" $ c_LoadIcon (maybePtr mb_inst) icon
foreign import stdcall unsafe "windows.h LoadIconW"
  c_LoadIcon :: HINSTANCE -> Icon -> IO HICON

#{enum Cursor, castUINTToPtr
 , iDC_ARROW        = (UINT)IDC_ARROW
 , iDC_IBEAM        = (UINT)IDC_IBEAM
 , iDC_WAIT         = (UINT)IDC_WAIT
 , iDC_CROSS        = (UINT)IDC_CROSS
 , iDC_UPARROW      = (UINT)IDC_UPARROW
 , iDC_SIZENWSE     = (UINT)IDC_SIZENWSE
 , iDC_SIZENESW     = (UINT)IDC_SIZENESW
 , iDC_SIZEWE       = (UINT)IDC_SIZEWE
 , iDC_SIZENS       = (UINT)IDC_SIZENS
 }

#{enum Icon, castUINTToPtr
 , iDI_APPLICATION  = (UINT)IDI_APPLICATION
 , iDI_HAND         = (UINT)IDI_HAND
 , iDI_QUESTION     = (UINT)IDI_QUESTION
 , iDI_EXCLAMATION  = (UINT)IDI_EXCLAMATION
 , iDI_ASTERISK     = (UINT)IDI_ASTERISK
 }

----------------------------------------------------------------
-- Message Boxes
----------------------------------------------------------------

type MBStyle = UINT

#{enum MBStyle,
 , mB_OK                = MB_OK
 , mB_OKCANCEL          = MB_OKCANCEL
 , mB_ABORTRETRYIGNORE  = MB_ABORTRETRYIGNORE
 , mB_YESNOCANCEL       = MB_YESNOCANCEL
 , mB_YESNO             = MB_YESNO
 , mB_RETRYCANCEL       = MB_RETRYCANCEL
 , mB_ICONHAND          = MB_ICONHAND
 , mB_ICONQUESTION      = MB_ICONQUESTION
 , mB_ICONEXCLAMATION   = MB_ICONEXCLAMATION
 , mB_ICONASTERISK      = MB_ICONASTERISK
 , mB_ICONINFORMATION   = MB_ICONINFORMATION
 , mB_ICONSTOP          = MB_ICONSTOP
 , mB_DEFBUTTON1        = MB_DEFBUTTON1
 , mB_DEFBUTTON2        = MB_DEFBUTTON2
 , mB_DEFBUTTON3        = MB_DEFBUTTON3
 , mB_APPLMODAL         = MB_APPLMODAL
 , mB_SYSTEMMODAL       = MB_SYSTEMMODAL
 , mB_TASKMODAL         = MB_TASKMODAL
 , mB_SETFOREGROUND     = MB_SETFOREGROUND
 }

type MBStatus = UINT

#{enum MBStatus,
 , iDABORT      = IDABORT
 , iDCANCEL     = IDCANCEL
 , iDIGNORE     = IDIGNORE
 , iDNO         = IDNO
 , iDOK         = IDOK
 , iDRETRY      = IDRETRY
 , iDYES        = IDYES
 }

-- Note: if the error is ever raised, we're in a very sad way!

messageBox :: HWND -> String -> String -> MBStyle -> IO MBStatus
messageBox wnd text caption style =
  withTString text $ \ c_text ->
  withTString caption $ \ c_caption ->
  failIfZero "MessageBox" $ c_MessageBox wnd c_text c_caption style
foreign import stdcall unsafe "windows.h MessageBoxW"
  c_MessageBox :: HWND -> LPCTSTR -> LPCTSTR -> MBStyle -> IO MBStatus

----------------------------------------------------------------
--
----------------------------------------------------------------

type StdHandleId   = DWORD

#{enum StdHandleId,
 , sTD_INPUT_HANDLE     = STD_INPUT_HANDLE
 , sTD_OUTPUT_HANDLE    = STD_OUTPUT_HANDLE
 , sTD_ERROR_HANDLE     = STD_ERROR_HANDLE
 }

iNVALID_HANDLE_VALUE :: HANDLE
iNVALID_HANDLE_VALUE = castUINTToPtr 0xffffffff

getStdHandle :: StdHandleId -> IO HANDLE
getStdHandle hid =
  failIf (== iNVALID_HANDLE_VALUE) "GetStdHandle" $ c_GetStdHandle hid
foreign import stdcall unsafe "windows.h GetStdHandle"
  c_GetStdHandle :: StdHandleId -> IO HANDLE

----------------------------------------------------------------
-- Rotatable Ellipse hack
--
-- Win95 (Win32?) doesn't support rotating ellipses - so we
-- implement them with polygons.
--
-- We use a fixed number of edges rather than varying the number
-- according to the radius of the ellipse.
-- If anyone feels like improving the code (to vary the number),
-- they should place a fixed upper bound on the number of edges
-- since it takes a relatively long time to draw 1000 edges.
----------------------------------------------------------------

transformedEllipse :: HDC -> POINT -> POINT -> POINT -> IO ()
transformedEllipse dc (x0,y0) (x1,y1) (x2,y2) =
  failIfFalse_ "transformedEllipse" $ c_transformedEllipse dc x0 y0 x1 y1 x2 y2
foreign import ccall unsafe "ellipse.h transformedEllipse"
  c_transformedEllipse :: HDC -> LONG -> LONG -> LONG -> LONG -> LONG -> LONG -> IO Bool

{-# CBITS ellipse.c #-}

----------------------------------------------------------------
-- Cursor
----------------------------------------------------------------

getCursorPos :: IO POINT
getCursorPos =
  allocaPOINT $ \ p_pt -> do
  failIfFalse_ "GetCursorPos" $ c_GetCursorPos p_pt
  peekPOINT p_pt
foreign import stdcall unsafe "windows.h GetCursorPos"
  c_GetCursorPos :: Ptr POINT -> IO Bool

setCursorPos :: POINT -> IO ()
setCursorPos (x,y) =
  failIfFalse_ "setCursorPos" $ c_SetCursorPos x y
foreign import stdcall unsafe "windows.h SetCursorPos"
  c_SetCursorPos :: LONG -> LONG -> IO Bool

clipCursor :: RECT -> IO ()
clipCursor rect =
  withRECT rect $ \ p_rect ->
  failIfFalse_ "ClipCursor" $ c_ClipCursor p_rect
foreign import stdcall unsafe "windows.h ClipCursor"
  c_ClipCursor :: Ptr RECT -> IO Bool

getClipCursor :: IO RECT
getClipCursor =
  allocaRECT $ \ p_rect -> do
  failIfFalse_ "GetClipCursor" $ c_GetClipCursor p_rect
  peekRECT p_rect
foreign import stdcall unsafe "windows.h GetClipCursor"
  c_GetClipCursor :: Ptr RECT -> IO Bool

----------------------------------------------------------------
-- Exit/shutdown
----------------------------------------------------------------

type ExitOption = UINT

#{enum ExitOption,
 , eWX_FORCE    = EWX_FORCE
 , eWX_LOGOFF   = EWX_LOGOFF
 , eWX_POWEROFF = EWX_POWEROFF
 , eWX_REBOOT   = EWX_REBOOT
 , eWX_SHUTDOWN = EWX_SHUTDOWN
 }

exitWindowsEx :: ExitOption -> IO ()
exitWindowsEx opt =
  failIfFalse_ "ExitWindowsEx" $ c_ExitWindowsEx opt 0
foreign import stdcall unsafe "windows.h ExitWindowsEx"
  c_ExitWindowsEx :: ExitOption -> DWORD -> IO Bool

exitWindows :: IO ()
exitWindows = exitWindowsEx 0

----------------------------------------------------------------
-- Beeping
----------------------------------------------------------------

type Beep = UINT
type MbBeep = Maybe Beep

maybeBeep :: Maybe Beep -> Beep
maybeBeep = maybe 0xffffffff id

type Duration   = Int

type MbDuration   = Maybe Duration

maybeDuration :: Maybe Duration -> Duration
maybeDuration = maybe (-1) id

messageBeep :: Maybe Beep -> IO ()
messageBeep mb_beep =
  c_MessageBeep (maybeBeep mb_beep)
foreign import stdcall unsafe "windows.h MessageBeep"
  c_MessageBeep :: Beep -> IO ()

beep :: WORD -> MbDuration -> IO ()
beep freq mb_dur =
  failIfFalse_ "Beep" $ c_Beep freq (maybeDuration mb_dur)
foreign import stdcall unsafe "windows.h Beep"
  c_Beep :: WORD -> Duration -> IO Bool

----------------------------------------------------------------
-- Timers
----------------------------------------------------------------

type TimerId   = UINT

type TIMERPROC = FunPtr (HWND -> UINT -> TimerId -> DWORD -> IO ())

-- ToDo: support the other two forms of timer initialisation

-- Cause WM_TIMER events to be sent to window callback

setWinTimer :: HWND -> TimerId -> UINT -> IO TimerId
setWinTimer wnd timer elapse =
  failIfZero "SetTimer" $ c_SetTimer wnd timer elapse nullFunPtr
foreign import stdcall unsafe "windows.h SetTimer"
  c_SetTimer :: HWND -> TimerId -> UINT -> TIMERPROC -> IO TimerId

killTimer :: Maybe HWND -> TimerId -> IO ()
killTimer mb_wnd timer =
  failIfFalse_ "KillTimer" $ c_KillTimer (maybePtr mb_wnd) timer
foreign import stdcall unsafe "windows.h KillTimer"
  c_KillTimer :: HWND -> TimerId -> IO Bool

-- For documentation purposes:
type MilliSeconds = DWORD

foreign import stdcall unsafe "windows.h timeGetTime"
  timeGetTime :: IO MilliSeconds

----------------------------------------------------------------

-- %fun ezCreateFont :: Unknown
-- %result BITMAP({ getBitmapInfo(x) })

----------------------------------------------------------------
-- End
----------------------------------------------------------------
