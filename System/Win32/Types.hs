-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.Types
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

module System.Win32.Types
	( module System.Win32.Types
	, nullPtr
	) where

import Data.Maybe
import Foreign
import Foreign.C
import Numeric (showHex)

----------------------------------------------------------------
-- Platform specific definitions
--
-- Most typedefs and prototypes in Win32 are expressed in terms
-- of these types.  Try to follow suit - it'll make it easier to
-- get things working on Win64 (or whatever they call it on Alphas).
----------------------------------------------------------------

type BOOL          = Bool
type BYTE          = Word8
type USHORT        = Word16
type UINT          = Word32
type INT           = Int32
type WORD          = Word16
type DWORD         = Word32
type LONG          = Int32
type FLOAT         = Float

----------------------------------------------------------------

type MbString      = Maybe String
type MbINT         = Maybe INT

type ATOM          = UINT
type WPARAM        = UINT
type LPARAM        = LONG
type LRESULT       = LONG

type MbATOM        = Maybe ATOM

----------------------------------------------------------------
-- Pointers
----------------------------------------------------------------

type Addr          = Ptr ()

type LPVOID        = Ptr ()
type LPBYTE        = Ptr BYTE
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

numToMaybe :: Num a => a -> Maybe a
numToMaybe n = if n == 0 then Nothing else Just n

type MbLPVOID      = Maybe LPVOID
type MbLPCSTR      = Maybe LPCSTR
type MbLPCTSTR     = Maybe LPCTSTR

----------------------------------------------------------------
-- Chars and strings
----------------------------------------------------------------

withTString    :: String -> (LPTSTR -> IO a) -> IO a
withTStringLen :: String -> ((LPTSTR, Int) -> IO a) -> IO a
peekTString    :: LPCTSTR -> IO String
peekTStringLen :: (LPCTSTR, Int) -> IO String
newTString     :: String -> IO LPCTSTR

-- UTF-16 version:
type TCHAR     = CWchar
withTString    = withCWString
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

----------------------------------------------------------------
-- Handles
----------------------------------------------------------------

type   HANDLE      = Ptr ()
type   ForeignHANDLE = ForeignPtr ()

newForeignHANDLE :: HANDLE -> IO ForeignHANDLE
newForeignHANDLE = newForeignPtr deleteObject_p

handleToWord :: HANDLE -> UINT
handleToWord = castPtrToUINT

type   HKEY        = ForeignHANDLE
type   PKEY        = HANDLE

nullHANDLE :: HANDLE
nullHANDLE = nullPtr

type MbHANDLE      = Maybe HANDLE

type   HINSTANCE   = Ptr ()
type MbHINSTANCE   = Maybe HINSTANCE

type   HMODULE     = Ptr ()
type MbHMODULE     = Maybe HMODULE

nullFinalHANDLE :: ForeignPtr a
nullFinalHANDLE = unsafePerformIO (newForeignPtr_ nullPtr)

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

failIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
failIfNull = failIf (== nullPtr)

failIfZero :: Num a => String -> IO a -> IO a
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

errorWin :: String -> IO a
errorWin fn_name = do
  err_code <- getLastError
  failWith fn_name err_code

failWith :: String -> ErrCode -> IO a
failWith fn_name err_code =
  withTString fn_name $ \ c_fn_name -> do
  c_msg <- getErrorMessage err_code
  msg <- peekTString c_msg
  localFree c_msg
  fail (fn_name ++ ": " ++ msg ++ " (error code: " ++ showHex err_code ")")

----------------------------------------------------------------
-- Primitives
----------------------------------------------------------------

foreign import stdcall unsafe "windows.h &DeleteObject"
  deleteObject_p :: FunPtr (HANDLE -> IO ())

foreign import stdcall unsafe "windows.h LocalFree"
  localFree :: Ptr a -> IO (Ptr a)

foreign import stdcall unsafe "windows.h GetLastError"
  getLastError :: IO ErrCode

{-# CFILES cbits/errors.c #-}

foreign import ccall unsafe "errors.h"
  getErrorMessage :: DWORD -> IO LPWSTR

{-# CFILES cbits/HsWin32.c #-}

foreign import ccall unsafe "HsWin32.h"
  lOWORD :: DWORD -> WORD

foreign import ccall unsafe "HsWin32.h"
  hIWORD :: DWORD -> WORD

foreign import ccall unsafe "HsWin32.h"
  castUINTToPtr :: UINT -> Ptr a

foreign import ccall unsafe "HsWin32.h"
  castPtrToUINT :: Ptr s -> UINT

foreign import ccall unsafe "HsWin32.h"
  castFunPtrToLONG :: FunPtr a -> LONG

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
