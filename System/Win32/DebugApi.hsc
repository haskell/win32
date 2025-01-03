#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.DebugApi
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
module System.Win32.DebugApi
    ( PID, TID, DebugEventId, ForeignAddress
    , PHANDLE, THANDLE
    , ThreadInfo
    , ImageInfo
    , ExceptionInfo
    , Exception(..)
    , DebugEventInfo(..)
    , DebugEvent

    , debugBreak
    , isDebuggerPresent

      -- * Debug events
    , waitForDebugEvent
    , getDebugEvents
    , continueDebugEvent

      -- * Debugging another process
    , debugActiveProcess
    , peekProcessMemory
    , readProcessMemory
    , pokeProcessMemory
    , withProcessMemory
    , peekP
    , pokeP

      -- * Thread control
    , suspendThread
    , resumeThread
    , withSuspendedThread

      -- * Thread register control
    , getThreadContext
    , setThreadContext
    , useAllRegs
    , withThreadContext

#if defined(i386_HOST_ARCH)
    , eax, ebx, ecx, edx, esi, edi, ebp, eip, esp
#elif defined(x86_64_HOST_ARCH)
    , rax, rbx, rcx, rdx, rsi, rdi, rbp, rip, rsp
#endif
#if defined(x86_64_HOST_ARCH) || defined(i386_HOST_ARCH)
    , segCs, segDs, segEs, segFs, segGs
    , eFlags
    , dr
#endif
#if defined(aarch64_HOST_ARCH)
    , x0,   x1,  x2,  x3,  x4,  x5,  x6,  x7,  x8
    , x9,  x10, x11, x12, x13, x14, x15, x16, x17
    , x18, x19, x20, x21, x22, x23, x24, x25, x26
    , x27, x28,  fp,  lr,  sp,  pc
#endif
    , setReg, getReg, modReg
    , makeModThreadContext
    , modifyThreadContext

      -- * Sending debug output to another process
    , outputDebugString
    ) where

import System.Win32.DebugApi.Internal
import Control.Exception( bracket_ )
import Foreign          ( Ptr, nullPtr, ForeignPtr, mallocForeignPtrBytes
                        , peekByteOff, plusPtr, allocaBytes, castPtr, poke
                        , withForeignPtr, Storable, sizeOf, peek, pokeByteOff )
import System.IO        ( fixIO )
import System.Win32.Types   ( WORD, DWORD, failIf_, failWith
                            , getLastError, failIf, withTString )

##include "windows_cconv.h"
#include "windows.h"



data Exception
    = UnknownException
    | AccessViolation Bool ForeignAddress
    | ArrayBoundsExceeded
    | Breakpoint
    | DataTypeMisalignment
    | FltDenormalOperand
    | FltDivideByZero
    | FltInexactResult
    | FltInvalidOperation
    | FltOverflow
    | FltStackCheck
    | FltUnderflow
    | IllegalInstruction
    | InPageError
    | IntDivideByZero
    | IntOverflow
    | InvalidDisposition
    | NonContinuable
    | PrivilegedInstruction
    | SingleStep
    | StackOverflow
    deriving (Show)

data DebugEventInfo
    = UnknownDebugEvent
    | Exception         ExceptionInfo Exception
    | CreateThread      ThreadInfo
    | CreateProcess     PHANDLE ImageInfo ThreadInfo
    | ExitThread        TID
    | ExitProcess       PID
    | LoadDll           ImageInfo
    | UnloadDll         TID
    | DebugString       ForeignAddress Bool WORD
    deriving (Show)

type DebugEvent = (DebugEventId, DebugEventInfo)

--------------------------------------------------------------------------
-- Handling debugevents

peekDebugEvent :: Ptr a -> IO DebugEvent
peekDebugEvent p = do
    code <- (#peek DEBUG_EVENT, dwDebugEventCode) p
    pid  <- (#peek DEBUG_EVENT, dwProcessId) p
    tid  <- (#peek DEBUG_EVENT, dwThreadId) p
    r <- rest (code::DWORD) (plusPtr p (#offset DEBUG_EVENT, u))
    return ((pid,tid), r)
    where
        dwZero = 0 :: DWORD
        wZero = 0 :: WORD

        rest (#const EXCEPTION_DEBUG_EVENT) p' = do
            chance  <- (#peek EXCEPTION_DEBUG_INFO, dwFirstChance) p'
            flags   <- (#peek EXCEPTION_RECORD, ExceptionFlags) p'
            addr    <- (#peek EXCEPTION_RECORD, ExceptionAddress) p'
            code    <- (#peek EXCEPTION_RECORD, ExceptionCode) p'
            e <- case code::DWORD of
                (#const EXCEPTION_ACCESS_VIOLATION)         -> return $ AccessViolation False 0
                (#const EXCEPTION_ARRAY_BOUNDS_EXCEEDED)    -> return ArrayBoundsExceeded
                (#const EXCEPTION_BREAKPOINT)               -> return Breakpoint
                (#const EXCEPTION_DATATYPE_MISALIGNMENT)    -> return DataTypeMisalignment
                (#const EXCEPTION_FLT_DENORMAL_OPERAND)     -> return FltDenormalOperand
                (#const EXCEPTION_FLT_DIVIDE_BY_ZERO)       -> return FltDivideByZero
                (#const EXCEPTION_FLT_INEXACT_RESULT)       -> return FltInexactResult
                (#const EXCEPTION_FLT_INVALID_OPERATION)    -> return FltInvalidOperation
                (#const EXCEPTION_FLT_OVERFLOW)             -> return FltOverflow
                (#const EXCEPTION_FLT_STACK_CHECK)          -> return FltStackCheck
                (#const EXCEPTION_FLT_UNDERFLOW)            -> return FltUnderflow
                (#const EXCEPTION_ILLEGAL_INSTRUCTION)      -> return IllegalInstruction
                (#const EXCEPTION_IN_PAGE_ERROR)            -> return InPageError
                (#const EXCEPTION_INT_DIVIDE_BY_ZERO)       -> return IntDivideByZero
                (#const EXCEPTION_INT_OVERFLOW)             -> return IntOverflow
                (#const EXCEPTION_INVALID_DISPOSITION)      -> return InvalidDisposition
                (#const EXCEPTION_NONCONTINUABLE_EXCEPTION) -> return NonContinuable
                (#const EXCEPTION_PRIV_INSTRUCTION)         -> return PrivilegedInstruction
                (#const EXCEPTION_SINGLE_STEP)              -> return SingleStep
                (#const EXCEPTION_STACK_OVERFLOW)           -> return StackOverflow
                _                                           -> return UnknownException
            return $ Exception (chance/=dwZero, flags==dwZero, addr) e

        rest (#const CREATE_THREAD_DEBUG_EVENT) p' = do
            handle <- (#peek CREATE_THREAD_DEBUG_INFO, hThread)          p'
            local <- (#peek CREATE_THREAD_DEBUG_INFO, lpThreadLocalBase) p'
            start <- (#peek CREATE_THREAD_DEBUG_INFO, lpStartAddress)    p'
            return $ CreateThread (handle, local, start)

        rest (#const CREATE_PROCESS_DEBUG_EVENT) p' = do
            file    <- (#peek CREATE_PROCESS_DEBUG_INFO, hFile) p'
            proc    <- (#peek CREATE_PROCESS_DEBUG_INFO, hProcess) p'
            thread  <- (#peek CREATE_PROCESS_DEBUG_INFO, hThread) p'
            imgbase <- (#peek CREATE_PROCESS_DEBUG_INFO, lpBaseOfImage) p'
            dbgoff  <- (#peek CREATE_PROCESS_DEBUG_INFO, dwDebugInfoFileOffset) p'
            dbgsize <- (#peek CREATE_PROCESS_DEBUG_INFO, nDebugInfoSize) p'
            local   <- (#peek CREATE_PROCESS_DEBUG_INFO, lpThreadLocalBase) p'
            start   <- (#peek CREATE_PROCESS_DEBUG_INFO, lpStartAddress) p'
            imgname <- (#peek CREATE_PROCESS_DEBUG_INFO, lpImageName) p'
            --unicode <- (#peek CREATE_PROCESS_DEBUG_INFO, fUnicode) p'
            return $ CreateProcess proc
                        (file, imgbase, dbgoff, dbgsize, imgname) --, unicode/=wZero)
                        (thread, local, start)

        rest (#const EXIT_THREAD_DEBUG_EVENT) p' =
            (#peek EXIT_THREAD_DEBUG_INFO, dwExitCode) p' >>= return.ExitThread

        rest (#const EXIT_PROCESS_DEBUG_EVENT) p' =
            (#peek EXIT_PROCESS_DEBUG_INFO, dwExitCode) p' >>= return.ExitProcess

        rest (#const LOAD_DLL_DEBUG_EVENT) p' = do
            file    <- (#peek LOAD_DLL_DEBUG_INFO, hFile) p'
            imgbase <- (#peek LOAD_DLL_DEBUG_INFO, lpBaseOfDll) p'
            dbgoff  <- (#peek LOAD_DLL_DEBUG_INFO, dwDebugInfoFileOffset) p'
            dbgsize <- (#peek LOAD_DLL_DEBUG_INFO, nDebugInfoSize) p'
            imgname <- (#peek LOAD_DLL_DEBUG_INFO, lpImageName) p'
            --unicode <- (#peek LOAD_DLL_DEBUG_INFO, fUnicode) p'
            return $
                LoadDll (file, imgbase, dbgoff, dbgsize, imgname)--, unicode/=wZero)

        rest (#const OUTPUT_DEBUG_STRING_EVENT) p' = do
            dat     <- (#peek OUTPUT_DEBUG_STRING_INFO, lpDebugStringData) p'
            unicode <- (#peek OUTPUT_DEBUG_STRING_INFO, fUnicode) p'
            len     <- (#peek OUTPUT_DEBUG_STRING_INFO, nDebugStringLength) p'
            return $ DebugString dat (unicode/=wZero) len

        rest (#const UNLOAD_DLL_DEBUG_EVENT) p' =
            (#peek UNLOAD_DLL_DEBUG_INFO, lpBaseOfDll) p' >>= return.UnloadDll

        rest _ _ = return UnknownDebugEvent



waitForDebugEvent :: Maybe Int -> IO (Maybe DebugEvent)
waitForDebugEvent timeout = allocaBytes (#size DEBUG_EVENT) $ \buf -> do
    res <- c_WaitForDebugEvent buf $ maybe (#const INFINITE) fromIntegral timeout
    if res
        then peekDebugEvent buf >>= return.Just
        else getLastError >>= \e -> case e of
            (#const ERROR_INVALID_HANDLE)   -> return Nothing
            (#const ERROR_SEM_TIMEOUT)      -> return Nothing
            _                               -> die e
    where
        die res = failWith "WaitForDebugEvent" res

getDebugEvents :: Int -> IO [DebugEvent]
getDebugEvents timeout = waitForDebugEvent (Just timeout) >>= getMore
    where
        getMore e = case e of
            Nothing -> return []
            Just e'  -> do
                rest <- waitForDebugEvent (Just 0) >>= getMore
                return $ e':rest

continueDebugEvent :: DebugEventId -> Bool -> IO ()
continueDebugEvent (pid,tid) cont =
    failIf_ not "ContinueDebugEvent" $ c_ContinueDebugEvent pid tid cont'
    where
        cont' = if cont
            then (#const DBG_CONTINUE)
            else (#const DBG_EXCEPTION_NOT_HANDLED)

--------------------------------------------------------------------------
-- Process control

debugActiveProcess :: PID -> IO ()
debugActiveProcess pid =
    failIf_ not "debugActiveProcess: DebugActiveProcess" $
        c_DebugActiveProcess pid

-- Windows XP
-- debugActiveProcessStop :: PID -> IO ()
-- debugActiveProcessStop pid =
--     failIf_ not "debugActiveProcessStop: DebugActiveProcessStop" $
--         c_DebugActiveProcessStop pid

--------------------------------------------------------------------------
-- Process memory

peekProcessMemory :: PHANDLE -> ForeignAddress -> Int -> Ptr a -> IO ()
peekProcessMemory proc addr size buf =
    failIf_ not "peekProcessMemory: ReadProcessMemory" $
        c_ReadProcessMemory proc (plusPtr nullPtr $ fromIntegral addr) (castPtr buf) (fromIntegral size) nullPtr

readProcessMemory :: PHANDLE -> ForeignAddress -> Int -> IO (ForeignPtr a)
readProcessMemory proc addr size = do
    res <- mallocForeignPtrBytes size
    withForeignPtr res $ peekProcessMemory proc addr size
    return res

pokeProcessMemory :: PHANDLE -> ForeignAddress -> Int -> Ptr a -> IO ()
pokeProcessMemory proc addr size buf =
    failIf_ not "pokeProcessMemory: WriteProcessMemory" $
        c_WriteProcessMemory proc (plusPtr nullPtr $ fromIntegral addr) (castPtr buf) (fromIntegral size) nullPtr

withProcessMemory :: PHANDLE -> ForeignAddress -> Int -> (Ptr a -> IO b) -> IO b
withProcessMemory proc addr size act = allocaBytes size $ \buf -> do
    peekProcessMemory proc addr size buf
    res <- act buf
    pokeProcessMemory proc addr size buf
    return res

peekP :: (Storable a) => PHANDLE -> ForeignAddress -> IO a
peekP proc addr = fixIO $ \res -> withProcessMemory proc addr (sizeOf res) peek

pokeP :: (Storable a) => PHANDLE -> ForeignAddress -> a -> IO ()
pokeP proc addr v = withProcessMemory proc addr (sizeOf v) $ \buf -> poke buf v

--------------------------------------------------------------------------
-- Thread Control

suspendThread :: THANDLE -> IO DWORD
suspendThread t =
    failIf (==0-1) "SuspendThread" $ c_SuspendThread t

resumeThread :: THANDLE -> IO DWORD
resumeThread t =
    failIf (==0-1) "ResumeThread" $ c_ResumeThread t

withSuspendedThread :: THANDLE -> IO a -> IO a
withSuspendedThread t = bracket_ (suspendThread t) (resumeThread t)

--getThreadId :: THANDLE -> IO TID
--getThreadId = failIf (==0) "GetThreadId" . c_GetThreadId

--------------------------------------------------------------------------
-- Thread register control
getThreadContext :: THANDLE -> Ptr a -> IO ()
getThreadContext t buf =
    failIf_ not "GetThreadContext" $ c_GetThreadContext t (castPtr buf)

setThreadContext :: THANDLE -> Ptr a -> IO ()
setThreadContext t buf =
    failIf_ not "SetThreadContext" $ c_SetThreadContext t (castPtr buf)

useAllRegs :: Ptr a -> IO ()
useAllRegs buf = (#poke CONTEXT, ContextFlags) buf v
    where
        v = (#const CONTEXT_FULL|CONTEXT_DEBUG_REGISTERS|CONTEXT_FLOATING_POINT) :: DWORD

withThreadContext :: THANDLE -> (Ptr a -> IO b) -> IO b
withThreadContext t act =
    allocaBytes (#size CONTEXT)
        $ \buf -> bracket_
            (useAllRegs buf >> getThreadContext t buf)
            (useAllRegs buf >> setThreadContext t buf)
            (act buf)


#if defined(i386_HOST_ARCH)
eax, ebx, ecx, edx :: Int
esi, edi :: Int
ebp, eip, esp :: Int
eax = (#offset CONTEXT, Eax)
ebx = (#offset CONTEXT, Ebx)
ecx = (#offset CONTEXT, Ecx)
edx = (#offset CONTEXT, Edx)
esi = (#offset CONTEXT, Esi)
edi = (#offset CONTEXT, Edi)
ebp = (#offset CONTEXT, Ebp)
eip = (#offset CONTEXT, Eip)
esp = (#offset CONTEXT, Esp)
#elif defined(x86_64_HOST_ARCH)
rax, rbx, rcx, rdx :: Int
rsi, rdi :: Int
rbp, rip, rsp :: Int
rax = (#offset CONTEXT, Rax)
rbx = (#offset CONTEXT, Rbx)
rcx = (#offset CONTEXT, Rcx)
rdx = (#offset CONTEXT, Rdx)
rsi = (#offset CONTEXT, Rsi)
rdi = (#offset CONTEXT, Rdi)
rbp = (#offset CONTEXT, Rbp)
rip = (#offset CONTEXT, Rip)
rsp = (#offset CONTEXT, Rsp)
#elif defined(aarch64_HOST_ARCH)
x0,   x1,  x2,  x3,  x4,  x5,  x6,  x7,  x8 :: Int
x9,  x10, x11, x12, x13, x14, x15, x16, x17 :: Int
x18, x19, x20, x21, x22, x23, x24, x25, x26 :: Int
x27, x28,  fp,  lr,  sp,  pc                :: Int
x0  = (#offset CONTEXT, X0 )
x1  = (#offset CONTEXT, X1 )
x2  = (#offset CONTEXT, X2 )
x3  = (#offset CONTEXT, X3 )
x4  = (#offset CONTEXT, X4 )
x5  = (#offset CONTEXT, X5 )
x6  = (#offset CONTEXT, X6 )
x7  = (#offset CONTEXT, X7 )
x8  = (#offset CONTEXT, X8 )
x9  = (#offset CONTEXT, X9 )
x10 = (#offset CONTEXT, X10)
x11 = (#offset CONTEXT, X11)
x12 = (#offset CONTEXT, X12)
x13 = (#offset CONTEXT, X13)
x14 = (#offset CONTEXT, X14)
x15 = (#offset CONTEXT, X15)
x16 = (#offset CONTEXT, X16)
x17 = (#offset CONTEXT, X17)
x18 = (#offset CONTEXT, X18)
x19 = (#offset CONTEXT, X19)
x20 = (#offset CONTEXT, X20)
x21 = (#offset CONTEXT, X21)
x22 = (#offset CONTEXT, X22)
x23 = (#offset CONTEXT, X23)
x24 = (#offset CONTEXT, X24)
x25 = (#offset CONTEXT, X25)
x26 = (#offset CONTEXT, X26)
x27 = (#offset CONTEXT, X27)
x28 = (#offset CONTEXT, X28)
fp  = (#offset CONTEXT, Fp)
lr  = (#offset CONTEXT, Lr)
sp  = (#offset CONTEXT, Sp)
pc  = (#offset CONTEXT, Pc)
#else
#error Unknown mingw32 arch
#endif

#if defined(x86_64_HOST_ARCH) || defined(i386_HOST_ARCH)
segCs, segDs, segEs, segFs, segGs :: Int
segCs = (#offset CONTEXT, SegCs)
segDs = (#offset CONTEXT, SegDs)
segEs = (#offset CONTEXT, SegEs)
segFs = (#offset CONTEXT, SegFs)
segGs = (#offset CONTEXT, SegGs)

eFlags :: Int
eFlags  = (#offset CONTEXT, EFlags)

dr :: Int -> Int
dr n = case n of
    0 -> (#offset CONTEXT, Dr0)
    1 -> (#offset CONTEXT, Dr1)
    2 -> (#offset CONTEXT, Dr2)
    3 -> (#offset CONTEXT, Dr3)
    6 -> (#offset CONTEXT, Dr6)
    7 -> (#offset CONTEXT, Dr7)
    _ -> undefined
#endif

setReg :: Ptr a -> Int -> DWORD -> IO ()
setReg = pokeByteOff

getReg :: Ptr a -> Int -> IO DWORD
getReg = peekByteOff

modReg :: Ptr a -> Int -> (DWORD->DWORD) -> IO DWORD
modReg buf r f = do
    old <- getReg buf r
    setReg buf r (f old)
    return old

makeModThreadContext :: [(Int, DWORD->DWORD)] -> Ptr a -> IO [DWORD]
makeModThreadContext act buf = mapM (uncurry $ modReg buf) act

modifyThreadContext :: THANDLE -> [(Int, DWORD->DWORD)] -> IO [DWORD]
modifyThreadContext t a = withThreadContext t $ makeModThreadContext a

--------------------------------------------------------------------------
-- On process being debugged

outputDebugString :: String -> IO ()
outputDebugString s = withTString s $ \c_s -> c_OutputDebugString c_s

