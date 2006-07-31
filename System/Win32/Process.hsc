-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Process
-- Copyright   :  (c) Alastair Reid, 1997-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Esa Ilari vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of FFI declarations for interfacing with Win32.
--
-----------------------------------------------------------------------------

module System.Win32.Process where
import Control.Exception    ( bracket )
import Control.Monad        ( liftM5 )
import Foreign              ( Ptr, peekByteOff, allocaBytes, pokeByteOff
                            , plusPtr )
import System.Win32.File    ( closeHandle )
import System.Win32.Types

#include <windows.h>
#include <tlhelp32.h>

-- constant to wait for a very long time.
iNFINITE :: DWORD
iNFINITE = #{const INFINITE}

foreign import stdcall unsafe "windows.h Sleep"
  sleep :: DWORD -> IO ()


type ProcessId = DWORD
type Th32SnapHandle = HANDLE
type Th32SnapFlags = DWORD
-- | ProcessId, number of threads, parent ProcessId, process base priority, path of executable file
type ProcessEntry32 = (ProcessId, Int, ProcessId, LONG, String)

#{enum Th32SnapFlags,
    , tH32CS_SNAPALL        = TH32CS_SNAPALL
    , tH32CS_SNAPHEAPLIST   = TH32CS_SNAPHEAPLIST
    , tH32CS_SNAPMODULE     = TH32CS_SNAPMODULE
    , tH32CS_SNAPPROCESS    = TH32CS_SNAPPROCESS
    , tH32CS_SNAPTHREAD     = TH32CS_SNAPTHREAD
    }
{-
    , tH32CS_SNAPGETALLMODS = TH32CS_GETALLMODS
    , tH32CS_SNAPNOHEAPS    = TH32CS_SNAPNOHEAPS 
-}

foreign import stdcall unsafe "tlhelp32.h CreateToolhelp32Snapshot"
    c_CreateToolhelp32Snapshot :: Th32SnapFlags -> ProcessId -> IO Th32SnapHandle

foreign import stdcall unsafe "tlhelp32.h Process32FirstW"
    c_Process32First :: Th32SnapHandle -> Ptr ProcessEntry32 -> IO BOOL

foreign import stdcall unsafe "tlhelp32.h Process32NextW"
    c_Process32Next :: Th32SnapHandle -> Ptr ProcessEntry32 -> IO BOOL

-- | Create a snapshot of specified resources.  Call closeHandle to close snapshot.
createToolhelp32Snapshot :: Th32SnapFlags -> Maybe ProcessId -> IO Th32SnapHandle
createToolhelp32Snapshot f p
    = failIfNull "CreateToolhelp32Snapshot" $ c_CreateToolhelp32Snapshot
        f (maybe 0 id p)

withTh32Snap :: Th32SnapFlags -> Maybe ProcessId -> (Th32SnapHandle -> IO a) -> IO a
withTh32Snap f p = bracket (createToolhelp32Snapshot f p) (closeHandle)


peekProcessEntry32 :: Ptr ProcessEntry32 -> IO ProcessEntry32
peekProcessEntry32 buf = liftM5 (,,,,)
    ((#peek PROCESSENTRY32, th32ProcessID) buf)
    ((#peek PROCESSENTRY32, cntThreads) buf)
    ((#peek PROCESSENTRY32, th32ParentProcessID) buf)
    ((#peek PROCESSENTRY32, pcPriClassBase) buf)
    (peekTString $ (#ptr PROCESSENTRY32, szExeFile) buf)

-- | Enumerate processes using Process32First and Process32Next
th32SnapEnumProcesses :: Th32SnapHandle -> IO [ProcessEntry32]
th32SnapEnumProcesses h = allocaBytes (#size PROCESSENTRY32) $ \pe -> do
    putStrLn "1"
    (#poke PROCESSENTRY32, dwSize) pe ((#size PROCESSENTRY32)::DWORD)
    putStrLn "2"
    ok <- c_Process32First h pe
    putStrLn "3"
    readAndNext ok pe []
    where
        readAndNext ok pe res
            | not ok    = do
                err <- getLastError
                print err
                if err==(#const ERROR_NO_MORE_FILES)
                    then return $ reverse res
                    else failWith "th32SnapEnumProcesses: Process32First/Process32Next" err
            | otherwise = do
                putStrLn "reading"
                entry <- peekProcessEntry32 pe
                ok' <- c_Process32Next h pe
                readAndNext ok' pe (entry:res)
