-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.WindowsString.DebugApi.Internal
-- Copyright   :  (c) Esa Ilari Vuokko, 2006
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for using Windows DebugApi.
--
-----------------------------------------------------------------------------

module System.Win32.DebugApi.Internal where

import Data.Word            ( Word8, Word32 )
import Foreign              ( Ptr )
import System.Win32.Types   ( BOOL, DWORD, HANDLE, LPTSTR )

##include "windows_cconv.h"
#include "windows.h"

type PID = DWORD
type TID = DWORD
type DebugEventId = (PID, TID)
type ForeignAddress = Word32

type PHANDLE = Ptr ()
type THANDLE = Ptr ()

type ThreadInfo = (THANDLE, ForeignAddress, ForeignAddress)   -- handle to thread, thread local, thread start
type ImageInfo = (HANDLE, ForeignAddress, DWORD, DWORD, ForeignAddress)
type ExceptionInfo = (Bool, Bool, ForeignAddress) -- First chance, continuable, address

--------------------------------------------------------------------------
-- Raw imports

foreign import WINDOWS_CCONV "windows.h SuspendThread"
    c_SuspendThread :: THANDLE -> IO DWORD

foreign import WINDOWS_CCONV "windows.h ResumeThread"
    c_ResumeThread :: THANDLE -> IO DWORD

foreign import WINDOWS_CCONV "windows.h WaitForDebugEvent"
    c_WaitForDebugEvent :: Ptr () -> DWORD -> IO BOOL

foreign import WINDOWS_CCONV "windows.h ContinueDebugEvent"
    c_ContinueDebugEvent :: DWORD -> DWORD -> DWORD -> IO BOOL

foreign import WINDOWS_CCONV "windows.h DebugActiveProcess"
    c_DebugActiveProcess :: DWORD -> IO Bool

-- Windows XP
-- foreign import WINDOWS_CCONV "windows.h DebugActiveProcessStop"
--     c_DebugActiveProcessStop :: DWORD -> IO Bool

foreign import WINDOWS_CCONV "windows.h ReadProcessMemory" c_ReadProcessMemory ::
    PHANDLE -> Ptr () -> Ptr Word8 -> DWORD -> Ptr DWORD -> IO BOOL

foreign import WINDOWS_CCONV "windows.h WriteProcessMemory" c_WriteProcessMemory ::
    PHANDLE -> Ptr () -> Ptr Word8 -> DWORD -> Ptr DWORD -> IO BOOL

foreign import WINDOWS_CCONV "windows.h GetThreadContext"
    c_GetThreadContext :: THANDLE -> Ptr () -> IO BOOL

foreign import WINDOWS_CCONV "windows.h SetThreadContext"
    c_SetThreadContext :: THANDLE -> Ptr () -> IO BOOL

--foreign import WINDOWS_CCONV "windows.h GetThreadId"
--    c_GetThreadId :: THANDLE -> IO TID

foreign import WINDOWS_CCONV "windows.h OutputDebugStringW"
    c_OutputDebugString :: LPTSTR -> IO ()

foreign import WINDOWS_CCONV "windows.h IsDebuggerPresent"
    isDebuggerPresent :: IO BOOL

foreign import WINDOWS_CCONV "windows.h  DebugBreak"
    debugBreak :: IO ()
