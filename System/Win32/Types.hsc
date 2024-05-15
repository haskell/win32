{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Types
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

module System.Win32.Types
        ( module System.Win32.Types
        , nullPtr
        ) where

import Control.Concurrent.MVar (readMVar)
import Control.Exception (bracket, throwIO)
import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.Char (isSpace)
import Data.Int (Int32, Int64, Int16)
import Data.Maybe (fromMaybe)
import Data.Typeable (cast)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C.Error (Errno(..), errnoToIOError)
import Foreign.C.String (newCWString, withCWStringLen, CWString)
import Foreign.C.String (peekCWString, peekCWStringLen, withCWString)
import Foreign.C.Types (CChar, CUChar, CWchar, CInt(..), CIntPtr(..), CUIntPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, newForeignPtr_)
import Foreign.Ptr (FunPtr, Ptr, nullPtr, ptrToIntPtr)
import Foreign.StablePtr (StablePtr, freeStablePtr, newStablePtr)
import Foreign (allocaArray)
import GHC.IO.Exception
import GHC.IO.FD (FD(..))
import GHC.IO.Handle.FD (fdToHandle)
import GHC.IO.Handle.Types (Handle(..), Handle__(..))
import Numeric (showHex)
import qualified System.IO as IO ()
import System.IO.Error (ioeSetErrorString)
import System.IO.Unsafe (unsafePerformIO)

#if !MIN_VERSION_base(4,8,0)
import Data.Word (Word)
#endif

#if MIN_VERSION_base(4,7,0)
import Data.Bits (finiteBitSize)
#else
import Data.Bits (Bits, bitSize)

finiteBitSize :: (Bits a) => a -> Int
finiteBitSize = bitSize
#endif

##if defined(__IO_MANAGER_WINIO__)
import Control.Monad (when, liftM2)
import Foreign.C.Types (CUIntPtr(..))
import Foreign.Marshal.Utils (fromBool, with)
import Foreign (peek)
import Foreign.Ptr (ptrToWordPtr)
import GHC.IO.SubSystem ((<!>))
import GHC.IO.Handle.Windows
import GHC.IO.IOMode
import GHC.IO.Windows.Handle (fromHANDLE, Io(), NativeHandle(), ConsoleHandle(),
                              toHANDLE, handleToMode, optimizeFileAccess)
import qualified GHC.Event.Windows as Mgr
import GHC.IO.Device (IODeviceType(..), devType)
##endif

#include <fcntl.h>
#include <windows.h>
##include "windows_cconv.h"

----------------------------------------------------------------
-- Platform specific definitions
--
-- Most typedefs and prototypes in Win32 are expressed in terms
-- of these types.  Try to follow suit - it'll make it easier to
-- get things working on Win64 (or whatever they call it on Alphas).
----------------------------------------------------------------

type BOOL          = Bool
type BYTE          = Word8
type UCHAR         = CUChar
type USHORT        = Word16
type UINT          = Word32
type INT           = Int32
type WORD          = Word16
type DWORD         = Word32
type LONG          = Int32
type FLOAT         = Float
type LARGE_INTEGER = Int64

type DWORD32       = Word32
type DWORD64       = Word64
type INT32         = Int32
type INT64         = Int64
type LONG32        = Int32
type LONG64        = Int64
type UINT32        = Word32
type UINT64        = Word64
type ULONG32       = Word32
type ULONG64       = Word64
type SHORT         = Int16

type INT_PTR       = Ptr CInt
type ULONG         = Word32
type UINT_PTR      = Word
type LONG_PTR      = CIntPtr
type ULONG_PTR     = CUIntPtr
type DWORD_PTR     = ULONG_PTR
#ifdef _WIN64
type HALF_PTR      = Ptr INT32
#else
type HALF_PTR      = Ptr SHORT
#endif

-- Not really a basic type, but used in many places
type DDWORD        = Word64

----------------------------------------------------------------

type MbString      = Maybe String
type MbINT         = Maybe INT

type ATOM          = WORD
type WPARAM        = UINT_PTR
type LPARAM        = LONG_PTR
type LRESULT       = LONG_PTR
type SIZE_T        = ULONG_PTR

type MbATOM        = Maybe ATOM

type HRESULT       = LONG

----------------------------------------------------------------
-- Pointers
----------------------------------------------------------------

type Addr          = Ptr ()

type LPVOID        = Ptr ()
type LPBOOL        = Ptr BOOL
type LPBYTE        = Ptr BYTE
type PUCHAR        = Ptr UCHAR
type LPDWORD       = Ptr DWORD
type LPSTR         = Ptr CChar
type LPCSTR        = LPSTR
type LPWSTR        = Ptr CWchar
type LPCWSTR       = LPWSTR
type LPTSTR        = Ptr TCHAR
type LPCTSTR       = LPTSTR
type LPCTSTR_      = LPCTSTR

-- Optional things with defaults

maybePtr :: Maybe (Ptr a) -> Ptr a
maybePtr = fromMaybe nullPtr

ptrToMaybe :: Ptr a -> Maybe (Ptr a)
ptrToMaybe p = if p == nullPtr then Nothing else Just p

maybeNum :: Num a => Maybe a -> a
maybeNum = fromMaybe 0

numToMaybe :: (Eq a, Num a) => a -> Maybe a
numToMaybe n = if n == 0 then Nothing else Just n

type MbLPVOID      = Maybe LPVOID
type MbLPCSTR      = Maybe LPCSTR
type MbLPCTSTR     = Maybe LPCTSTR

----------------------------------------------------------------
-- Chars and strings
----------------------------------------------------------------

withTString    :: String -> (LPTSTR -> IO a) -> IO a
withFilePath   :: FilePath -> (LPTSTR -> IO a) -> IO a
withTStringLen :: String -> ((LPTSTR, Int) -> IO a) -> IO a
peekTString    :: LPCTSTR -> IO String
peekTStringLen :: (LPCTSTR, Int) -> IO String
newTString     :: String -> IO LPCTSTR

-- UTF-16 version:
type TCHAR     = CWchar
withTString    = withCWString
withFilePath path = useAsCWStringSafe path
withTStringLen = withCWStringLen
peekTString    = peekCWString
peekTStringLen = peekCWStringLen
newTString     = newCWString

{- ANSI version:
type TCHAR     = CChar
withTString    = withCString
withTStringLen = withCStringLen
peekTString    = peekCString
peekTStringLen = peekCStringLen
newTString     = newCString
-}

-- | Wrapper around 'useAsCString', checking the encoded 'FilePath' for internal NUL codepoints as these are
-- disallowed in Windows filepaths. See https://gitlab.haskell.org/ghc/ghc/-/issues/13660
useAsCWStringSafe :: FilePath -> (CWString -> IO a) -> IO a
useAsCWStringSafe path f =
    if '\NUL' `elem` path
    then ioError err
    else withCWString path f
  where
    err =
        IOError
          { ioe_handle = Nothing
          , ioe_type = InvalidArgument
          , ioe_location = "useAsCWStringSafe"
          , ioe_description = "Windows filepaths must not contain internal NUL codepoints."
          , ioe_errno = Nothing
          , ioe_filename = Just path
          }


----------------------------------------------------------------
-- Handles
----------------------------------------------------------------

type   HANDLE      = Ptr ()
type   ForeignHANDLE = ForeignPtr ()

newForeignHANDLE :: HANDLE -> IO ForeignHANDLE
newForeignHANDLE = newForeignPtr deleteObjectFinaliser

handleToWord :: HANDLE -> UINT_PTR
handleToWord = castPtrToUINTPtr

type   HKEY        = ForeignHANDLE
type   PKEY        = HANDLE

nullHANDLE :: HANDLE
nullHANDLE = nullPtr

type MbHANDLE      = Maybe HANDLE

nullHINSTANCE :: HINSTANCE
nullHINSTANCE = nullPtr

type   HINSTANCE   = Ptr ()
type MbHINSTANCE   = Maybe HINSTANCE

type   HMODULE     = Ptr ()
type MbHMODULE     = Maybe HMODULE

nullFinalHANDLE :: ForeignPtr a
nullFinalHANDLE = unsafePerformIO (newForeignPtr_ nullPtr)

iNVALID_HANDLE_VALUE :: HANDLE
iNVALID_HANDLE_VALUE = castUINTPtrToPtr maxBound

iNVALID_SET_FILE_POINTER :: DWORD
iNVALID_SET_FILE_POINTER = #const INVALID_SET_FILE_POINTER

foreign import ccall "_open_osfhandle"
  _open_osfhandle :: CIntPtr -> CInt -> IO CInt

-- | Create a Haskell 'Handle' from a Windows 'HANDLE'.
--
-- Beware that this function allocates a new file descriptor. A consequence of
-- this is that calling 'hANDLEToHandle' on the standard Windows handles will
-- not give you 'IO.stdin', 'IO.stdout', or 'IO.stderr'. For example, if you
-- run this code:
--
-- @
-- import Graphics.Win32.Misc
-- stdoutHANDLE <- getStdHandle sTD_OUTPUT_HANDLE
-- stdout2 <- 'hANDLEToHandle' stdoutHANDLE
-- @
--
-- Then although you can use @stdout2@ to write to standard output, it is not
-- the case that @'IO.stdout' == stdout2@.
hANDLEToHandle :: HANDLE -> IO Handle
hANDLEToHandle handle = posix
##if defined(__IO_MANAGER_WINIO__)
     <!> native
##endif
  where
##if defined(__IO_MANAGER_WINIO__)
    native = do
      -- Attach the handle to the I/O manager's CompletionPort.  This allows the
      -- I/O manager to service requests for this Handle.
      Mgr.associateHandle' handle
      let hwnd = fromHANDLE handle :: Io NativeHandle
      _type <- devType hwnd

      -- Use the rts to enforce any file locking we may need.
      mode <- handleToMode handle
      let write_lock = mode /= ReadMode

      case _type of
        -- Regular files need to be locked.
        -- See also Note [RTS File locking]
        RegularFile -> do
          optimizeFileAccess handle -- Set a few optimization flags on file handles.
          (unique_dev, unique_ino) <- getUniqueFileInfo handle
          r <- internal_lockFile
                  (fromIntegral $ ptrToWordPtr handle) unique_dev unique_ino
                  (fromBool write_lock)
          when (r == -1)  $
               ioException (IOError Nothing ResourceBusy "hANDLEToHandle"
                                  "file is locked" Nothing Nothing)

        -- I don't see a reason for blocking directories.  So unlike the FD
        -- implementation I'll allow it.
        _ -> return ()
      mkHandleFromHANDLE hwnd Stream ("hwnd:" ++ show handle) mode Nothing

    -- | getUniqueFileInfo assumes the C call to getUniqueFileInfo
    -- succeeds.
    getUniqueFileInfo :: HANDLE -> IO (Word64, Word64)
    getUniqueFileInfo hnl = do
      with 0 $ \devptr -> do
        with 0 $ \inoptr -> do
          internal_getUniqueFileInfo hnl devptr inoptr
          liftM2 (,) (peek devptr) (peek inoptr)
##endif
    posix = _open_osfhandle (fromIntegral (ptrToIntPtr handle))
                            (#const _O_BINARY) >>= fdToHandle

##if defined(__IO_MANAGER_WINIO__)
foreign import ccall unsafe "lockFile"
  internal_lockFile :: CUIntPtr -> Word64 -> Word64 -> CInt -> IO CInt

-- | Returns -1 on error. Otherwise writes two values representing
-- the file into the given ptrs.
foreign import ccall unsafe "get_unique_file_info_hwnd"
  internal_getUniqueFileInfo :: HANDLE -> Ptr Word64 -> Ptr Word64 -> IO ()
##endif

foreign import ccall unsafe "_get_osfhandle"
  c_get_osfhandle :: CInt -> IO HANDLE

-- | Extract a Windows 'HANDLE' from a Haskell 'Handle' and perform
-- an action on it.

-- Originally authored by Max Bolingbroke in the ansi-terminal library
withHandleToHANDLE :: Handle -> (HANDLE -> IO a) -> IO a
##if defined(__IO_MANAGER_WINIO__)
withHandleToHANDLE = withHandleToHANDLEPosix <!> withHandleToHANDLENative
##else
withHandleToHANDLE = withHandleToHANDLEPosix
##endif

##if defined(__IO_MANAGER_WINIO__)
withHandleToHANDLENative :: Handle -> (HANDLE -> IO a) -> IO a
withHandleToHANDLENative haskell_handle action =
    -- Create a stable pointer to the Handle. This prevents the garbage collector
    -- getting to it while we are doing horrible manipulations with it, and hence
    -- stops it being finalized (and closed).
    withStablePtr haskell_handle $ const $ do
        -- Grab the write handle variable from the Handle
        let write_handle_mvar = case haskell_handle of
                FileHandle _ handle_mvar     -> handle_mvar
                DuplexHandle _ _ handle_mvar -> handle_mvar

        -- This is "write" MVar, we could also take the "read" one
        windows_handle <- readMVar write_handle_mvar >>= handle_ToHANDLE

        -- Do what the user originally wanted
        action windows_handle
  where
    -- | Turn an existing Handle into a Win32 HANDLE. This function throws an
    -- IOError if the Handle does not reference a HANDLE
    handle_ToHANDLE :: Handle__ -> IO HANDLE
    handle_ToHANDLE (Handle__{haDevice = dev}) =
        case (cast dev :: Maybe (Io NativeHandle), cast dev :: Maybe (Io ConsoleHandle)) of
          (Just hwnd, Nothing) -> return $ toHANDLE hwnd
          (Nothing, Just hwnd) -> return $ toHANDLE hwnd
          _                    -> throwErr "not a known HANDLE"

    throwErr msg = ioException $ IOError (Just haskell_handle)
      InappropriateType "withHandleToHANDLENative" msg Nothing Nothing
##endif

withHandleToHANDLEPosix :: Handle -> (HANDLE -> IO a) -> IO a
withHandleToHANDLEPosix haskell_handle action =
    -- Create a stable pointer to the Handle. This prevents the garbage collector
    -- getting to it while we are doing horrible manipulations with it, and hence
    -- stops it being finalized (and closed).
    withStablePtr haskell_handle $ const $ do
        -- Grab the write handle variable from the Handle
        let write_handle_mvar = case haskell_handle of
                FileHandle _ handle_mvar     -> handle_mvar
                DuplexHandle _ _ handle_mvar -> handle_mvar
                  -- This is "write" MVar, we could also take the "read" one

        -- Get the FD from the algebraic data type
        Just fd <- fmap (\(Handle__ { haDevice = dev }) -> fmap fdFD (cast dev))
                 $ readMVar write_handle_mvar

        -- Finally, turn that (C-land) FD into a HANDLE using msvcrt
        windows_handle <- c_get_osfhandle fd
        -- Do what the user originally wanted
        action windows_handle

withStablePtr :: a -> (StablePtr a -> IO b) -> IO b
withStablePtr value = bracket (newStablePtr value) freeStablePtr

----------------------------------------------------------------
-- Errors
----------------------------------------------------------------

type ErrCode = DWORD

failIf :: (a -> Bool) -> String -> IO a -> IO a
failIf p wh act = do
  v <- act
  if p v then errorWin wh else return v

failIf_ :: (a -> Bool) -> String -> IO a -> IO ()
failIf_ p wh act = do
  v <- act
  if p v then errorWin wh else return ()

failIfNeg :: (Num a, Ord a) => String -> IO a -> IO a
failIfNeg = failIf (< 0)

failIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
failIfNull = failIf (== nullPtr)

failIfZero :: (Eq a, Num a) => String -> IO a -> IO a
failIfZero = failIf (== 0)

failIfFalse_ :: String -> IO Bool -> IO ()
failIfFalse_ = failIf_ not

failUnlessSuccess :: String -> IO ErrCode -> IO ()
failUnlessSuccess fn_name act = do
  r <- act
  if r == 0 then return () else failWith fn_name r

failUnlessSuccessOr :: ErrCode -> String -> IO ErrCode -> IO Bool
failUnlessSuccessOr val fn_name act = do
  r <- act
  if r == 0 then return False
    else if r == val then return True
    else failWith fn_name r

eRROR_INSUFFICIENT_BUFFER :: ErrCode
eRROR_INSUFFICIENT_BUFFER = #const ERROR_INSUFFICIENT_BUFFER

eRROR_MOD_NOT_FOUND :: ErrCode
eRROR_MOD_NOT_FOUND = #const ERROR_MOD_NOT_FOUND

eRROR_PROC_NOT_FOUND :: ErrCode
eRROR_PROC_NOT_FOUND = #const ERROR_PROC_NOT_FOUND

eERROR_ENVVAR_NOT_FOUND :: ErrCode
eERROR_ENVVAR_NOT_FOUND = #const ERROR_ENVVAR_NOT_FOUND

errorWin :: String -> IO a
errorWin fn_name = do
  err_code <- getLastError
  failWith fn_name err_code

failWith :: String -> ErrCode -> IO a
failWith fn_name err_code = do
  c_msg <- getErrorMessage err_code
  msg <- if c_msg == nullPtr
           then return $ "Error 0x" ++ Numeric.showHex err_code ""
           else do msg <- peekTString c_msg
                   -- We ignore failure of freeing c_msg, given we're already failing
                   _ <- localFree c_msg
                   return msg
  -- turn GetLastError() into errno, which errnoToIOError knows how to convert
  -- to an IOException we can throw.
  errno <- c_maperrno_func err_code
  let msg' = reverse $ dropWhile isSpace $ reverse msg -- drop trailing \n
      ioerror = errnoToIOError fn_name errno Nothing Nothing
                  `ioeSetErrorString` msg'
  throwIO ioerror


foreign import ccall unsafe "maperrno_func" -- in base/cbits/Win32Utils.c
   c_maperrno_func :: ErrCode -> IO Errno

----------------------------------------------------------------
-- Misc helpers
----------------------------------------------------------------

ddwordToDwords :: DDWORD -> (DWORD,DWORD)
ddwordToDwords n =
        (fromIntegral (n `shiftR` finiteBitSize (undefined :: DWORD))
        ,fromIntegral (n .&. fromIntegral (maxBound :: DWORD)))

dwordsToDdword:: (DWORD,DWORD) -> DDWORD
dwordsToDdword (hi,low) = (fromIntegral low) .|. (fromIntegral hi `shiftL` finiteBitSize hi)

-- Support for API calls that are passed a fixed-size buffer and tell
-- you via the return value if the buffer was too small.  In that
-- case, we double the buffer size and try again.
try :: String -> (LPTSTR -> UINT -> IO UINT) -> UINT -> IO String
try loc f n = do
   e <- allocaArray (fromIntegral n) $ \lptstr -> do
          r <- failIfZero loc $ f lptstr n
          if (r > n) then return (Left r) else do
            str <- peekTStringLen (lptstr, fromIntegral r)
            return (Right str)
   case e of
        Left n'   -> try loc f n'
        Right str -> return str

----------------------------------------------------------------
-- Primitives
----------------------------------------------------------------

{-# CFILES cbits/HsWin32.c #-}
foreign import ccall "HsWin32.h &DeleteObjectFinaliser"
  deleteObjectFinaliser :: FunPtr (Ptr a -> IO ())

foreign import WINDOWS_CCONV unsafe "windows.h LocalFree"
  localFree :: Ptr a -> IO (Ptr a)

foreign import WINDOWS_CCONV unsafe "windows.h GetLastError"
  getLastError :: IO ErrCode

foreign import WINDOWS_CCONV unsafe "windows.h SetLastError"
  setLastError :: ErrCode -> IO ()

{-# CFILES cbits/errors.c #-}

foreign import ccall unsafe "errors.h"
  getErrorMessage :: DWORD -> IO LPWSTR

{-# CFILES cbits/HsWin32.c #-}

foreign import ccall unsafe "HsWin32.h"
  lOWORD :: DWORD -> WORD

foreign import ccall unsafe "HsWin32.h"
  hIWORD :: DWORD -> WORD

foreign import ccall unsafe "HsWin32.h"
  castUINTPtrToPtr :: UINT_PTR -> Ptr a

foreign import ccall unsafe "HsWin32.h"
  castPtrToUINTPtr :: Ptr s -> UINT_PTR

type LCID = DWORD

type LANGID = WORD
type SortID = WORD

foreign import ccall unsafe "HsWin32.h prim_MAKELCID"
  mAKELCID :: LANGID -> SortID -> LCID

foreign import ccall unsafe "HsWin32.h prim_LANGIDFROMLCID"
  lANGIDFROMLCID :: LCID -> LANGID

foreign import ccall unsafe "HsWin32.h prim_SORTIDFROMLCID"
  sORTIDFROMLCID :: LCID -> SortID

type SubLANGID = WORD
type PrimaryLANGID = WORD

foreign import ccall unsafe "HsWin32.h prim_MAKELANGID"
  mAKELANGID :: PrimaryLANGID -> SubLANGID -> LANGID

foreign import ccall unsafe "HsWin32.h prim_PRIMARYLANGID"
  pRIMARYLANGID :: LANGID -> PrimaryLANGID

foreign import ccall unsafe "HsWin32.h prim_SUBLANGID"
  sUBLANGID :: LANGID -> SubLANGID

----------------------------------------------------------------
-- End
----------------------------------------------------------------
