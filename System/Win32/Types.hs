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

import Data.Char (chr, ord)
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

foreign import ccall unsafe "HsWin32.h"
  lOWORD :: DWORD -> WORD

foreign import ccall unsafe "HsWin32.h"
  hIWORD :: DWORD -> WORD

----------------------------------------------------------------

type MbString      = Maybe String
type MbINT         = Maybe INT

type ATOM          = UINT
type WPARAM        = UINT
type LPARAM        = LONG
type LRESULT       = LONG

type MbATOM        = Maybe ATOM

type WCHAR         = Word16

----------------------------------------------------------------
-- Pointers
----------------------------------------------------------------

type Addr          = Ptr ()

type LPVOID        = Ptr ()
type LPBYTE        = Ptr BYTE
type LPSTR         = Ptr CChar
type LPCSTR        = LPSTR
type LPWSTR        = Ptr WCHAR
type LPCWSTR       = LPWSTR
type LPTSTR        = Ptr TCHAR
type LPCTSTR       = LPTSTR
type LPCTSTR_      = LPCTSTR

-- Optional things with defaults

withMaybePtr :: (a -> (Ptr b -> IO c) -> IO c) ->
                Maybe a -> (Ptr b -> IO c) -> IO c
withMaybePtr _withX Nothing f = f nullPtr
withMaybePtr withX (Just x) f = withX x f

withMaybeForeignPtr :: Maybe (ForeignPtr a) -> (Ptr a -> IO b) -> IO b
withMaybeForeignPtr = withMaybePtr withForeignPtr

maybePtr :: Maybe (Ptr a) -> Ptr a
maybePtr = maybe nullPtr id

ptrToMaybe :: Ptr a -> Maybe (Ptr a)
ptrToMaybe p = if p == nullPtr then Nothing else Just p

maybeNum :: Num a => Maybe a -> a
maybeNum = maybe 0 id

numToMaybe :: Num a => a -> Maybe a
numToMaybe n = if n == 0 then Nothing else Just n

foreign import ccall unsafe "HsWin32.h"
  castUINTToPtr :: UINT -> Ptr a

foreign import ccall unsafe "HsWin32.h"
  castPtrToUINT :: Ptr s -> UINT

foreign import ccall unsafe "HsWin32.h"
  castFunPtrToLONG :: FunPtr a -> LONG

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
type TCHAR     = WCHAR
withTString    = withWString
withTStringLen = withWStringLen
peekTString    = peekWString
peekTStringLen = peekWStringLen
newTString     = newWString

{- ANSI version:
type TCHAR     = CChar
withTString    = withCString
withTStringLen = withCStringLen
peekTString    = peekCString
peekTStringLen = peekCStringLen
newTString     = newCString
-}

toUTF16 :: String -> [WCHAR]
toUTF16 = foldr utf16Char [] . map ord
 where
  utf16Char c wcs
    | c < 0x10000 = fromIntegral c : wcs
    | otherwise   = let c' = c - 0x10000 in
                    fromIntegral (c' `div` 0x400 + 0xd800) :
                    fromIntegral (c' `mod` 0x400 + 0xdc00) : wcs

-- coding errors generate Chars in the surrogate range
fromUTF16 :: [WCHAR] -> String
fromUTF16 = map chr . fromUTF16' . map fromIntegral
 where
  fromUTF16' (c1:c2:wcs)
    | 0xd800 <= c1 && c1 <= 0xdbff && 0xdc00 <= c2 && c2 <= 0xdfff =
      ((c1 - 0xd800)*0x400 + (c2 - 0xdc00) + 0x10000) : fromUTF16' wcs
  fromUTF16' (c:wcs) =
    c : fromUTF16' wcs
  fromUTF16' [] =
    []

withWString :: String -> (Ptr WCHAR -> IO a) -> IO a
withWString str =
  withArray0 0 (toUTF16 str)

withWStringLen :: String -> ((Ptr WCHAR, Int) -> IO a) -> IO a
withWStringLen str f =
  withArray (toUTF16 str) $ \ c_str ->
  f (c_str, length str)

peekWString :: Ptr WCHAR -> IO String
peekWString c_str = do
  wcs <- peekArray0 0 c_str
  return (fromUTF16 wcs)

peekWStringLen :: (Ptr WCHAR, Int) -> IO String
peekWStringLen (c_str, len) = do
  wcs <- peekArray len c_str
  return (fromUTF16 wcs)

newWString :: String -> IO LPCTSTR
newWString str =
  newArray0 0 (toUTF16 str)

----------------------------------------------------------------
-- Handles
----------------------------------------------------------------

type   HANDLE      = Ptr ()
type   ForeignHANDLE = ForeignPtr ()

newForeignHANDLE :: HANDLE -> IO ForeignHANDLE
newForeignHANDLE = newForeignPtr deleteObject_p

foreign import ccall unsafe "windows.h &DeleteObject"
  deleteObject_p :: FunPtr (HANDLE -> IO ())

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

{-# CBITS errors.c #-}

foreign import ccall unsafe "errors.h"
  getErrorMessage :: DWORD -> IO LPWSTR

foreign import ccall unsafe "windows.h LocalFree"
  localFree :: Ptr a -> IO (Ptr a)

foreign import ccall unsafe "windows.h GetLastError"
  getLastError :: IO ErrCode

----------------------------------------------------------------
-- End
----------------------------------------------------------------
