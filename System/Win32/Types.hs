#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
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

import Data.Maybe
import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe

#include "windows_cconv.h"

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

type UINT_PTR      = Word

-- Not really a basic type, but used in many places
type DDWORD        = Word64

----------------------------------------------------------------

type MbString      = Maybe String
type MbINT         = Maybe INT

type ATOM          = UINT
type WPARAM        = UINT
type LPARAM        = LONG
type LRESULT       = LONG
type SIZE_T        = DWORD
type CodePage      = UINT

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
newForeignHANDLE = newForeignPtr deleteObjectFinaliser

handleToWord :: HANDLE -> UINT_PTR
handleToWord = castPtrToUINTPtr

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

iNVALID_HANDLE_VALUE :: HANDLE
iNVALID_HANDLE_VALUE = castUINTPtrToPtr (-1)

----------------------------------------------------------------
-- Misc helpers
----------------------------------------------------------------

ddwordToDwords :: DDWORD -> (DWORD,DWORD)
ddwordToDwords n =
        (fromIntegral (n `shiftR` finiteBitSize (undefined :: DWORD))
        ,fromIntegral (n .&. fromIntegral (maxBound :: DWORD)))

dwordsToDdword:: (DWORD,DWORD) -> DDWORD
dwordsToDdword (hi,low) = (fromIntegral low) .|. (fromIntegral hi `shiftL` finiteBitSize hi)

----------------------------------------------------------------
-- Primitives
----------------------------------------------------------------

{-# CFILES cbits/HsWin32.c #-}
foreign import ccall "HsWin32.h &DeleteObjectFinaliser"
  deleteObjectFinaliser :: FunPtr (Ptr a -> IO ())

foreign import WINDOWS_CCONV unsafe "windows.h LocalFree"
  localFree :: Ptr a -> IO (Ptr a)

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
