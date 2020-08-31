{-# LANGUAGE Trustworthy #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.NLS
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

module System.Win32.NLS  (
        module System.Win32.NLS,

        -- defined in System.Win32.Types
        LCID, LANGID, SortID, SubLANGID, PrimaryLANGID,
        mAKELCID, lANGIDFROMLCID, sORTIDFROMLCID,
        mAKELANGID, pRIMARYLANGID, sUBLANGID
        ) where

import System.Win32.String (withTStringBufferLen)
import System.Win32.Types
import System.Win32.Utils (trySized)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif
import Foreign
import Foreign.C
import Text.Printf (printf)

##include "windows_cconv.h"

-- Somewhere, WINVER and _WIN32_WINNT are being defined as less than 0x0600 -
-- that is, before Windows Vista. Support for Windows XP was dropped in
-- GHC 8.0.1 of May 2016. This forces them to be at least 0x0600.
#if WINVER < 0x0600
#undef WINVER
#define WINVER 0x0600
#endif
#if _WIN32_WINNT < 0x0600
#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0600
#endif

#include <windows.h>
#include "alignment.h"
#include "errors.h"
#include "win32debug.h"
#include "winnls_compat.h"
#include "winnt_compat.h"

type NLS_FUNCTION = DWORD

#{enum LCID,
 , lOCALE_SYSTEM_DEFAULT = LOCALE_SYSTEM_DEFAULT
 , lOCALE_USER_DEFAULT   = LOCALE_USER_DEFAULT
 , lOCALE_NEUTRAL        = LOCALE_NEUTRAL
 }

foreign import WINDOWS_CCONV unsafe "windows.h ConvertDefaultLocale"
  convertDefaultLocale :: LCID -> IO LCID

-- ToDo: various enum functions.

type CodePage = UINT

#{enum CodePage,
 , cP_ACP       = CP_ACP
 , cP_MACCP     = CP_MACCP
 , cP_OEMCP     = CP_OEMCP
 }

foreign import WINDOWS_CCONV unsafe "windows.h GetACP"
  getACP :: IO CodePage

foreign import WINDOWS_CCONV unsafe "windows.h SetThreadLocale"
  setThreadLocale :: LCID -> IO ()

type LCTYPE = UINT

-- The following locale information constants are excluded from the `enum` list
-- below, for the reason indicated:
-- LOCALE_IDIALINGCODE -- Introduced in Windows 10 but not supported. Synonym
                       -- for LOCALE_ICOUNTRY.
-- LOCALE_INEGATIVEPERCENT -- Introduced in Windows 7 but not supported.
-- LOCALE_IPOSITIVEPERCENT -- Introduced in Windows 7 but not supported.
-- LOCALE_IREADINGLAYOUT -- Introduced in Windows 7 but not supported.
-- LOCALE_SAM -- Introduced by Windows 10 but not supported. Synonyn for
              -- LOCALE_S1159.
-- LOCALE_SENGLISHDISPLAYNAME -- Introduced in Windows 7 but not supported.
-- LOCALE_SIETFLANGUAGE -- Not supported (deprecated from Windows Vista).
-- LOCALE_SNATIVEDISPLAYNAME -- Introduced in Windows 7 but not supported.
-- LOCALE_SNATIVELANGUAGENAME -- Introduced in Windows 7 but not supported.
-- LOCALE_SPERCENT -- Introduced in Windows 7 but not supported.
-- LOCALE_SPM -- Introduced in Windows 10 but not supported. Synonym for
              -- LOCALE_S2359.
-- LOCALE_SSHORTESTAM -- Not supported.
-- LOCALE_SSHORTESTPM -- Not supported.
-- LOCALE_SSHORTTIME -- Introduced in Windows 7 but not supported.

-- The following locale information constant is included in the list below, but
-- note:
-- LOCALE_IINTLCURRDIGITS -- Not supported by Windows 10, use
                          -- LOCALE_ICURRDIGITS.

#{enum LCTYPE,
 , lOCALE_FONTSIGNATURE = LOCALE_FONTSIGNATURE
 , lOCALE_ICALENDARTYPE = LOCALE_ICALENDARTYPE
 , lOCALE_ICENTURY      = LOCALE_ICENTURY
 , lOCALE_ICOUNTRY      = LOCALE_ICOUNTRY
 , lOCALE_ICURRDIGITS   = LOCALE_ICURRDIGITS
 , lOCALE_ICURRENCY     = LOCALE_ICURRENCY
 , lOCALE_IDATE         = LOCALE_IDATE
 , lOCALE_IDAYLZERO     = LOCALE_IDAYLZERO
 , lOCALE_IDEFAULTANSICODEPAGE = LOCALE_IDEFAULTANSICODEPAGE
 , lOCALE_IDEFAULTCODEPAGE = LOCALE_IDEFAULTCODEPAGE
 , lOCALE_IDEFAULTCOUNTRY = LOCALE_IDEFAULTCOUNTRY
 , lOCALE_IDEFAULTEBCDICCODEPAGE = LOCALE_IDEFAULTEBCDICCODEPAGE
 , lOCALE_IDEFAULTLANGUAGE = LOCALE_IDEFAULTLANGUAGE
 , lOCALE_IDEFAULTMACCODEPAGE = LOCALE_IDEFAULTMACCODEPAGE
 , lOCALE_IDIGITS       = LOCALE_IDIGITS
 , lOCALE_IDIGITSUBSTITUTION = LOCALE_IDIGITSUBSTITUTION
 , lOCALE_IFIRSTDAYOFWEEK = LOCALE_IFIRSTDAYOFWEEK
 , lOCALE_IFIRSTWEEKOFYEAR = LOCALE_IFIRSTWEEKOFYEAR
 , lOCALE_IGEOID        = LOCALE_IGEOID
 , lOCALE_IINTLCURRDIGITS = LOCALE_IINTLCURRDIGITS
 , lOCALE_ILANGUAGE     = LOCALE_ILANGUAGE
 , lOCALE_ILDATE        = LOCALE_ILDATE
 , lOCALE_ILZERO        = LOCALE_ILZERO
 , lOCALE_IMEASURE      = LOCALE_IMEASURE
 , lOCALE_IMONLZERO     = LOCALE_IMONLZERO
 , lOCALE_INEGCURR      = LOCALE_INEGCURR
 , lOCALE_INEGNUMBER    = LOCALE_INEGNUMBER
 , lOCALE_INEGSEPBYSPACE = LOCALE_INEGSEPBYSPACE
 , lOCALE_INEGSIGNPOSN   = LOCALE_INEGSIGNPOSN
 , lOCALE_INEGSYMPRECEDES = LOCALE_INEGSYMPRECEDES
 , lOCALE_IOPTIONALCALENDAR = LOCALE_IOPTIONALCALENDAR
 , lOCALE_PAPERSIZE     = LOCALE_IPAPERSIZE
 , lOCALE_IPOSSEPBYSPACE = LOCALE_IPOSSEPBYSPACE
 , lOCALE_IPOSSIGNPOSN  = LOCALE_IPOSSIGNPOSN
 , lOCALE_IPSSYMPRECEDES = LOCALE_IPOSSYMPRECEDES
 , lOCALE_ITIME         = LOCALE_ITIME
 , lOCALE_ITIMEMARKPOSN = LOCALE_ITIMEMARKPOSN
 , lOCALE_ITLZERO       = LOCALE_ITLZERO
 , lOCALE_RETURN_NUMBER = LOCALE_RETURN_NUMBER
 , lOCALE_S1159         = LOCALE_S1159
 , lOCALE_S2359         = LOCALE_S2359
 , lOCALE_SABBREVCTRYNAME = LOCALE_SABBREVCTRYNAME
 , lOCALE_SABBREVDAYNAME1 = LOCALE_SABBREVDAYNAME1
 , lOCALE_SABBREVDAYNAME2 = LOCALE_SABBREVDAYNAME2
 , lOCALE_SABBREVDAYNAME3 = LOCALE_SABBREVDAYNAME3
 , lOCALE_SABBREVDAYNAME4 = LOCALE_SABBREVDAYNAME4
 , lOCALE_SABBREVDAYNAME5 = LOCALE_SABBREVDAYNAME5
 , lOCALE_SABBREVDAYNAME6 = LOCALE_SABBREVDAYNAME6
 , lOCALE_SABBREVDAYNAME7 = LOCALE_SABBREVDAYNAME7
 , lOCALE_SABBREVLANGNAME = LOCALE_SABBREVLANGNAME
 , lOCALE_SABBREVMONTHNAME1 = LOCALE_SABBREVMONTHNAME1
 , lOCALE_SABBREVMONTHNAME2 = LOCALE_SABBREVMONTHNAME2
 , lOCALE_SABBREVMONTHNAME3 = LOCALE_SABBREVMONTHNAME3
 , lOCALE_SABBREVMONTHNAME4 = LOCALE_SABBREVMONTHNAME4
 , lOCALE_SABBREVMONTHNAME5 = LOCALE_SABBREVMONTHNAME5
 , lOCALE_SABBREVMONTHNAME6 = LOCALE_SABBREVMONTHNAME6
 , lOCALE_SABBREVMONTHNAME7 = LOCALE_SABBREVMONTHNAME7
 , lOCALE_SABBREVMONTHNAME8 = LOCALE_SABBREVMONTHNAME8
 , lOCALE_SABBREVMONTHNAME9 = LOCALE_SABBREVMONTHNAME9
 , lOCALE_SABBREVMONTHNAME10 = LOCALE_SABBREVMONTHNAME10
 , lOCALE_SABBREVMONTHNAME11 = LOCALE_SABBREVMONTHNAME11
 , lOCALE_SABBREVMONTHNAME12 = LOCALE_SABBREVMONTHNAME12
 , lOCALE_SABBREVMONTHNAME13 = LOCALE_SABBREVMONTHNAME13
 , lOCALE_SCONSOLEFALLBACKNAME = LOCALE_SCONSOLEFALLBACKNAME
 , lOCALE_SCURRENCY     = LOCALE_SCURRENCY
 , lOCALE_SDATE         = LOCALE_SDATE
 , lOCALE_SDAYNAME1     = LOCALE_SDAYNAME1
 , lOCALE_SDAYNAME2     = LOCALE_SDAYNAME2
 , lOCALE_SDAYNAME3     = LOCALE_SDAYNAME3
 , lOCALE_SDAYNAME4     = LOCALE_SDAYNAME4
 , lOCALE_SDAYNAME5     = LOCALE_SDAYNAME5
 , lOCALE_SDAYNAME6     = LOCALE_SDAYNAME6
 , lOCALE_SDAYNAME7     = LOCALE_SDAYNAME7
 , lOCALE_SDECIMAL      = LOCALE_SDECIMAL
 , lOCALE_SDURATION     = LOCALE_SDURATION
 , lOCALE_SENGCURRNAME  = LOCALE_SENGCURRNAME
 , lOCALE_SENGLISHCOUNTRYNAME = LOCALE_SENGLISHCOUNTRYNAME
 , lOCALE_SENGLISHLANGUAGENAME = LOCALE_SENGLISHLANGUAGENAME
 , lOCALE_SGROUPING     = LOCALE_SGROUPING
 , lOCALE_SINTLSYMBOL   = LOCALE_SINTLSYMBOL
 , lOCALE_SISO3166CTRYNAME = LOCALE_SISO3166CTRYNAME
 , lOCALE_SISO3166CTRYNAME2 = LOCALE_SISO3166CTRYNAME2
 , lOCALE_SISO639LANGNAME = LOCALE_SISO639LANGNAME
 , lOCALE_SISO639LANGNAME2 = LOCALE_SISO639LANGNAME2
 , lOCALE_SKEYBOARDSTOINSTALL = LOCALE_SKEYBOARDSTOINSTALL
 , lOCALE_SLIST         = LOCALE_SLIST
 , lOCALE_SLONGDATE     = LOCALE_SLONGDATE
 , lOCALE_SMONDECIMALSEP = LOCALE_SMONDECIMALSEP
 , lOCALE_SMONGROUPING  = LOCALE_SMONGROUPING
 , lOCALE_SMONTHNAME1   = LOCALE_SMONTHNAME1
 , lOCALE_SMONTHNAME2   = LOCALE_SMONTHNAME2
 , lOCALE_SMONTHNAME3   = LOCALE_SMONTHNAME3
 , lOCALE_SMONTHNAME4   = LOCALE_SMONTHNAME4
 , lOCALE_SMONTHNAME5   = LOCALE_SMONTHNAME5
 , lOCALE_SMONTHNAME6   = LOCALE_SMONTHNAME6
 , lOCALE_SMONTHNAME7   = LOCALE_SMONTHNAME7
 , lOCALE_SMONTHNAME8   = LOCALE_SMONTHNAME8
 , lOCALE_SMONTHNAME9   = LOCALE_SMONTHNAME9
 , lOCALE_SMONTHNAME10  = LOCALE_SMONTHNAME10
 , lOCALE_SMONTHNAME11  = LOCALE_SMONTHNAME11
 , lOCALE_SMONTHNAME12  = LOCALE_SMONTHNAME12
 , lOCALE_SMONTHNAME13  = LOCALE_SMONTHNAME13
 , lOCALE_SMONTHOUSANDSEP = LOCALE_SMONTHOUSANDSEP
 , lOCALE_SNAME         = LOCALE_SNAME
 , lOCALE_SNAN          = LOCALE_SNAN
 , lOCALE_SNATIVECOUNTRYNAME = LOCALE_SNATIVECOUNTRYNAME
 , lOCALE_SNATIVECURRNAME = LOCALE_SNATIVECURRNAME
 , lOCALE_SNATIVEDIGITS = LOCALE_SNATIVEDIGITS
 , lOCALE_SNEGATIVESIGN = LOCALE_SNEGATIVESIGN
 , lOCALE_SNEGINFINITY  = LOCALE_SNEGINFINITY
 , lOCALE_SPARENT       = LOCALE_SPARENT
 , lOCALE_SPOSINFINITY  = LOCALE_SPOSINFINITY
 , lOCALE_SPOSITIVESIGN = LOCALE_SPOSITIVESIGN
 , lOCALE_SSCRIPTS      = LOCALE_SSCRIPTS
 , lOCALE_SSHORTDATE    = LOCALE_SSHORTDATE
 , lOCALE_SSHORTESTDAYNAME1 = LOCALE_SSHORTESTDAYNAME1
 , lOCALE_SSHORTESTDAYNAME2 = LOCALE_SSHORTESTDAYNAME2
 , lOCALE_SSHORTESTDAYNAME3 = LOCALE_SSHORTESTDAYNAME3
 , lOCALE_SSHORTESTDAYNAME4 = LOCALE_SSHORTESTDAYNAME4
 , lOCALE_SSHORTESTDAYNAME5 = LOCALE_SSHORTESTDAYNAME5
 , lOCALE_SSHORTESTDAYNAME6 = LOCALE_SSHORTESTDAYNAME6
 , lOCALE_SSHORTESTDAYNAME7 = LOCALE_SSHORTESTDAYNAME7
 , lOCALE_SSORTNAME     = LOCALE_SSORTNAME
 , lOCALE_STHOUSAND     = LOCALE_STHOUSAND
 , lOCALE_STIME         = LOCALE_STIME
 , lOCALE_STIMEFORMAT   = LOCALE_STIMEFORMAT
 , lOCALE_SYEARMONTH    = LOCALE_SYEARMONTH
 }

-- |Type representing locale data
data LCData
  -- | Data in the form of a Unicode string.
  = LCTextualData !String
  -- | Data in the form of a number. See 'lOCAL_RETURN_NUMBER' and @LOCAL_I*@
  -- locate information constants.
  | LCNumericData !DWORD
  -- | Data in the fomr of a 'LOCALESIGNATURE'. See 'lOCAL_FONTSIGNATURE' locale
  -- information constant.
  | LCSignatureData !LOCALESIGNATURE
  deriving (Eq, Show)

data LOCALESIGNATURE = LOCALESIGNATURE
  { lsUsb :: !UnicodeSubsetBitfield
  , lsCsbDefault :: !DDWORD
  , lsCsbSupported :: !DDWORD
  } deriving (Eq, Show)

instance Storable LOCALESIGNATURE where
  sizeOf = const #{size LOCALESIGNATURE}
  alignment _ = #alignment LOCALESIGNATURE
  peek buf = do
    lsUsb'          <- (#peek LOCALESIGNATURE, lsUsb) buf
    lsCsbDefault'   <- (#peek LOCALESIGNATURE, lsCsbDefault) buf
    lsCsbSupported' <- (#peek LOCALESIGNATURE, lsCsbSupported) buf
    return $ LOCALESIGNATURE lsUsb' lsCsbDefault' lsCsbSupported'
  poke buf info = do
    (#poke LOCALESIGNATURE, lsUsb) buf (lsUsb info)
    (#poke LOCALESIGNATURE, lsCsbDefault) buf (lsCsbDefault info)
    (#poke LOCALESIGNATURE, lsCsbSupported) buf (lsCsbSupported info)

-- | Type representing 128-bit Unicode subset bitfields, as the @base@ package
-- does include a module exporting a 128-bit unsigned integer type.
data UnicodeSubsetBitfield = UnicodeSubsetBitfield
  { usbLow :: !DDWORD
  , usbHigh :: !DDWORD
  } deriving (Eq, Show)

instance Storable UnicodeSubsetBitfield where
  sizeOf _ = 2 * sizeOf (undefined :: DDWORD)
  alignment _ = alignment (undefined :: DWORD)
  peek buf = do
    usbLow'  <- (peekByteOff buf 0 :: IO DDWORD)
    usbHigh' <- (peekByteOff buf (sizeOf (undefined :: DDWORD)) :: IO DDWORD)
    return $ UnicodeSubsetBitfield usbLow' usbHigh'
  poke buf info = do
    pokeByteOff buf 0 (usbLow info)
    pokeByteOff buf (sizeOf (undefined :: DDWORD)) (usbHigh info)

getLocaleInfoEx :: Maybe String -> LCTYPE -> IO LCData
getLocaleInfoEx locale ty
  | ty == lOCALE_FONTSIGNATURE =
      getLocaleInfoEx' LCSignatureData localSigCharCount

  | ty .&. lOCALE_RETURN_NUMBER /= 0 =
      getLocaleInfoEx' LCNumericData dWORDCharCount

  | otherwise = maybeWith withTString locale $ \c_locale ->
      LCTextualData <$> trySized cFuncName (c_GetLocaleInfoEx c_locale ty)
 where
  cFuncName = "GetLocaleInfoEx"
  -- See https://docs.microsoft.com/en-us/windows/win32/api/winnls/nf-winnls-getlocaleinfoex
  localSigCharCount = (#size LOCALESIGNATURE) `div` (#size WCHAR)
  dWORDCharCount = (#size DWORD) `div` (#size WCHAR)

  getLocaleInfoEx' constructor bufSize = maybeWith withTString locale $
    \c_locale -> do
      value <- alloca $ \buf -> do
        _ <- failIfZero cFuncName
          $ c_GetLocaleInfoEx c_locale ty (castPtr buf) bufSize
        peek buf
      return $ constructor value

foreign import WINDOWS_CCONV unsafe "windows.h GetLocaleInfoEx"
  c_GetLocaleInfoEx :: LPCWSTR -> LCTYPE -> LPWSTR -> CInt -> IO CInt

setLocaleInfo :: LCID -> LCTYPE -> String -> IO ()
setLocaleInfo locale ty info =
  withTString info $ \ c_info ->
  failIfFalse_ "SetLocaleInfo" $ c_SetLocaleInfo locale ty c_info
foreign import WINDOWS_CCONV unsafe "windows.h SetLocaleInfoW"
  c_SetLocaleInfo :: LCID -> LCTYPE -> LPCTSTR -> IO Bool

type LCMapFlags = DWORD

#{enum LCMapFlags,
 , lCMAP_BYTEREV              = LCMAP_BYTEREV
 , lCMAP_FULLWIDTH            = LCMAP_FULLWIDTH
 , lCMAP_HALFWIDTH            = LCMAP_HALFWIDTH
 , lCMAP_HIRAGANA             = LCMAP_HIRAGANA
 , lCMAP_KATAKANA             = LCMAP_KATAKANA
 , lCMAP_LINGUISTIC_CASING    = LCMAP_LINGUISTIC_CASING
 , lCMAP_LOWERCASE            = LCMAP_LOWERCASE
 , lCMAP_SIMPLIFIED_CHINESE   = LCMAP_SIMPLIFIED_CHINESE
 , lCMAP_SORTKEY              = LCMAP_SORTKEY
 , lCMAP_TRADITIONAL_CHINESE  = LCMAP_TRADITIONAL_CHINESE
 , lCMAP_UPPERCASE            = LCMAP_UPPERCASE
 , lINGUISTIC_IGNORECASE      = LINGUISTIC_IGNORECASE
 , lINGUISTIC_IGNOREDIACRITIC = LINGUISTIC_IGNOREDIACRITIC
 , nORM_IGNORECASE            = NORM_IGNORECASE
 , nORM_IGNORENONSPACE        = NORM_IGNORENONSPACE
 , nORM_IGNOREKANATYPE        = NORM_IGNOREKANATYPE
 , nORM_IGNORESYMBOLS         = NORM_IGNORESYMBOLS
 , nORM_IGNOREWIDTH           = NORM_IGNOREWIDTH
 , nORM_LINGUISTIC_CASING     = NORM_LINGUISTIC_CASING
 , sORT_STRINGSORT            = SORT_STRINGSORT
 }

data NLSVERSIONINFOEX = NLSVERSIONINFOEX
  { dwNLSVersionInfoSize :: DWORD
  , dwNLSVersion :: DWORD
  , dwDefinedVersion :: DWORD
  , dwEffectiveId :: DWORD
  , guidCustomVersion :: GUID
  } deriving (Eq, Show)

instance Storable NLSVERSIONINFOEX where
  sizeOf = const #{size NLSVERSIONINFOEX}
  alignment _ = #alignment NLSVERSIONINFOEX
  peek buf = do
    dwNLSVersionInfoSize' <- (#peek NLSVERSIONINFOEX, dwNLSVersionInfoSize) buf
    dwNLSVersion' <- (#peek NLSVERSIONINFOEX, dwNLSVersion) buf
    dwDefinedVersion' <- (#peek NLSVERSIONINFOEX, dwDefinedVersion) buf
    dwEffectiveId' <- (#peek NLSVERSIONINFOEX, dwEffectiveId) buf
    guidCustomVersion' <- (#peek NLSVERSIONINFOEX, guidCustomVersion) buf
    return $ NLSVERSIONINFOEX dwNLSVersionInfoSize' dwNLSVersion'
               dwDefinedVersion' dwEffectiveId' guidCustomVersion'
  poke buf info = do
    (#poke NLSVERSIONINFOEX, dwNLSVersionInfoSize) buf
      (dwNLSVersionInfoSize info)
    (#poke NLSVERSIONINFOEX, dwNLSVersion) buf (dwNLSVersion info)
    (#poke NLSVERSIONINFOEX, dwDefinedVersion) buf (dwDefinedVersion info)
    (#poke NLSVERSIONINFOEX, dwEffectiveId) buf (dwEffectiveId info)
    (#poke NLSVERSIONINFOEX, guidCustomVersion) buf (guidCustomVersion info)

-- Based on the `UnpackedUUID` type of package `uuid-types`.
data GUID = GUID
  !Word32
  !Word16
  !Word16
  !Word8
  !Word8
  !Word8
  !Word8
  !Word8
  !Word8
  !Word8
  !Word8
  deriving (Eq)

instance Show GUID where
  show (GUID data1 data2 data3 b1 b2 b3 b4 b5 b6 b7 b8) =
    printf "{%.8x-%.4x-%.4x-%.2x%2x-%.2x%.2x%.2x%.2x%.2x%.2x}" data1 data2 data3 b1 b2 b3 b4 b5 b6 b7 b8

instance Storable GUID where
  sizeOf _ = 16
  alignment _ = 4
  peekByteOff p off = GUID
    <$> peekByteOff p off
    <*> peekByteOff p (off + 4)
    <*> peekByteOff p (off + 6)
    <*> peekByteOff p (off + 8)
    <*> peekByteOff p (off + 9)
    <*> peekByteOff p (off + 10)
    <*> peekByteOff p (off + 11)
    <*> peekByteOff p (off + 12)
    <*> peekByteOff p (off + 13)
    <*> peekByteOff p (off + 14)
    <*> peekByteOff p (off + 15)
  pokeByteOff p off (GUID data1 data2 data3 b1 b2 b3 b4 b5 b6 b7 b8) = do
    pokeByteOff p off data1
    pokeByteOff p (off + 4) data2
    pokeByteOff p (off + 6) data3
    pokeByteOff p (off + 8) b1
    pokeByteOff p (off + 9) b2
    pokeByteOff p (off + 10) b3
    pokeByteOff p (off + 11) b4
    pokeByteOff p (off + 12) b5
    pokeByteOff p (off + 13) b6
    pokeByteOff p (off + 14) b7
    pokeByteOff p (off + 15) b8

getNLSVersionEx :: Maybe String -> IO NLSVERSIONINFOEX
getNLSVersionEx locale = maybeWith withTString locale $ \c_locale ->
  with defaultVersionInfo $ \c_versionInfo -> do
    failIfFalse_ "GetNLSVersionEx" $
      c_GetNLSVersionEx function c_locale c_versionInfo
    peek c_versionInfo
 where
  function = #{const COMPARE_STRING}
  defaultVersionInfo = NLSVERSIONINFOEX
    { dwNLSVersionInfoSize = #{size NLSVERSIONINFOEX}
    , dwNLSVersion = 0
    , dwDefinedVersion = 0
    , dwEffectiveId = 0
    , guidCustomVersion = GUID 0 0 0 0 0 0 0 0 0 0 0
    }
foreign import WINDOWS_CCONV unsafe "windows.h GetNLSVersionEx"
  c_GetNLSVersionEx :: NLS_FUNCTION
                    -> LPCWSTR
                    -> Ptr NLSVERSIONINFOEX
                    -> IO Bool

lCMapStringEx :: Maybe String
              -> LCMapFlags
              -> String
              -> NLSVERSIONINFOEX
              -> IO String
lCMapStringEx locale flags src versionInfo =
  maybeWith withTString locale $ \c_locale ->
    withTStringLen src $ \(c_src, src_len) ->
      with versionInfo $ \c_versionInfo -> do
        let c_src_len = fromIntegral src_len
            c_func s l = c_LCMapStringEx c_locale
                                         flags
                                         c_src c_src_len
                                         s l
                                         c_versionInfo
                                         nullPtr -- Reserved, must be NULL
                                         0 -- Reserved, must be 0
        trySized "LCMapStringEx" c_func
foreign import WINDOWS_CCONV unsafe "windows.h LCMapStringEx"
  c_LCMapStringEx :: LPCWSTR
                  -> LCMapFlags
                  -> LPCWSTR
                  -> CInt
                  -> LPWSTR
                  -> CInt
                  -> Ptr NLSVERSIONINFOEX
                  -> LPVOID
                  -> LPARAM
                  -> IO CInt

lCMapString :: LCID -> LCMapFlags -> String -> Int -> IO String
lCMapString locale flags src dest_size =
  withTStringLen src $ \ (c_src, src_len) ->
  allocaArray dest_size $ \ c_dest -> do
  _ <- failIfZero "LCMapString" $
    c_LCMapString locale flags c_src src_len c_dest dest_size
  peekTString c_dest
foreign import WINDOWS_CCONV unsafe "windows.h LCMapStringW"
  c_LCMapString :: LCID -> LCMapFlags -> LPCTSTR -> Int -> LPCTSTR -> Int -> IO Int

type LocaleTestFlags = DWORD

#{enum LocaleTestFlags,
 , lCID_INSTALLED       = LCID_INSTALLED
 , lCID_SUPPORTED       = LCID_SUPPORTED
 }

isValidLocalName :: Maybe String -> IO Bool
isValidLocalName lpLocaleName =
  maybeWith withTString lpLocaleName c_IsValidLocaleName
foreign import WINDOWS_CCONV unsafe "windows.h IsValidLocaleName"
  c_IsValidLocaleName :: LPCWSTR -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h IsValidLocale"
  isValidLocale :: LCID -> LocaleTestFlags -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h IsValidCodePage"
  isValidCodePage :: CodePage -> IO Bool

foreign import WINDOWS_CCONV unsafe "windows.h GetUserDefaultLCID"
  getUserDefaultLCID :: LCID

foreign import WINDOWS_CCONV unsafe "windows.h GetUserDefaultLangID"
  getUserDefaultLangID :: LANGID

-- #define LOCALE_NAME_INVARIANT L""
lOCALE_NAME_INVARIANT :: Maybe String
lOCALE_NAME_INVARIANT = Just ""

 -- #define LOCALE_NAME_SYSTEM_DEFAULT L"!x-sys-default-locale"
lOCALE_NAME_SYSTEM_DEFAULT :: Maybe String
lOCALE_NAME_SYSTEM_DEFAULT = Just "!x-sys-default-locale"

 -- #define LOCALE_NAME_USER_DEFAULT NULL
lOCALE_NAME_USER_DEFAULT :: Maybe String
lOCALE_NAME_USER_DEFAULT = Nothing

getUserDefaultLocaleName :: IO String
getUserDefaultLocaleName =
  getDefaultLocaleName "GetUserDefaultLocaleName" c_GetUserDefaultLocaleName
foreign import WINDOWS_CCONV unsafe "windows.h GetUserDefaultLocaleName"
  c_GetUserDefaultLocaleName :: LPWSTR -> CInt -> IO CInt

#{enum CInt,
 , lOCALE_NAME_MAX_LENGTH = LOCALE_NAME_MAX_LENGTH
 }

-- |Helper function for use with 'c_GetUserDefaultLocaleName' or
-- 'c_GetSystemDefaultLocaleName'. See 'getUserDefaultLocaleName' and
-- 'getSystemUserDefaultLocaleName'.
getDefaultLocaleName :: String -> (LPWSTR -> CInt -> IO CInt) -> IO String
getDefaultLocaleName cDefaultLocaleFuncName cDefaultLocaleFunc =
  withTStringBufferLen maxLength $ \(buf, len) -> do
    let c_len = fromIntegral len
    c_len' <- failIfZero cDefaultLocaleFuncName $
      cDefaultLocaleFunc buf c_len
    let len' = fromIntegral c_len'
    peekTStringLen (buf, len' - 1) -- Drop final null character
 where
  maxLength = fromIntegral lOCALE_NAME_MAX_LENGTH

foreign import WINDOWS_CCONV unsafe "windows.h GetThreadLocale"
  getThreadLocale :: IO LCID

foreign import WINDOWS_CCONV unsafe "windows.h GetSystemDefaultLCID"
  getSystemDefaultLCID :: LCID

foreign import WINDOWS_CCONV unsafe "windows.h GetSystemDefaultLangID"
  getSystemDefaultLangID :: LANGID

getSystemDefaultLocaleName :: IO String
getSystemDefaultLocaleName =
  getDefaultLocaleName "GetSystemDefaultLocaleName" c_GetSystemDefaultLocaleName
foreign import WINDOWS_CCONV unsafe "windows.h GetSystemDefaultLocaleName"
  c_GetSystemDefaultLocaleName :: LPWSTR -> CInt -> IO CInt

foreign import WINDOWS_CCONV unsafe "windows.h GetOEMCP"
  getOEMCP :: CodePage

#{enum PrimaryLANGID,
 , lANG_NEUTRAL         = LANG_NEUTRAL
 , lANG_BULGARIAN       = LANG_BULGARIAN
 , lANG_CHINESE         = LANG_CHINESE
 , lANG_CZECH           = LANG_CZECH
 , lANG_DANISH          = LANG_DANISH
 , lANG_GERMAN          = LANG_GERMAN
 , lANG_GREEK           = LANG_GREEK
 , lANG_ENGLISH         = LANG_ENGLISH
 , lANG_SPANISH         = LANG_SPANISH
 , lANG_FINNISH         = LANG_FINNISH
 , lANG_FRENCH          = LANG_FRENCH
 , lANG_HUNGARIAN       = LANG_HUNGARIAN
 , lANG_ICELANDIC       = LANG_ICELANDIC
 , lANG_ITALIAN         = LANG_ITALIAN
 , lANG_JAPANESE        = LANG_JAPANESE
 , lANG_KOREAN          = LANG_KOREAN
 , lANG_DUTCH           = LANG_DUTCH
 , lANG_NORWEGIAN       = LANG_NORWEGIAN
 , lANG_POLISH          = LANG_POLISH
 , lANG_PORTUGUESE      = LANG_PORTUGUESE
 , lANG_ROMANIAN        = LANG_ROMANIAN
 , lANG_RUSSIAN         = LANG_RUSSIAN
 , lANG_CROATIAN        = LANG_CROATIAN
 , lANG_SLOVAK          = LANG_SLOVAK
 , lANG_SWEDISH         = LANG_SWEDISH
 , lANG_TURKISH         = LANG_TURKISH
 , lANG_SLOVENIAN       = LANG_SLOVENIAN
 , lANG_ARABIC          = LANG_ARABIC
 , lANG_CATALAN         = LANG_CATALAN
 , lANG_HEBREW          = LANG_HEBREW
 , lANG_SERBIAN         = LANG_SERBIAN
 , lANG_ALBANIAN        = LANG_ALBANIAN
 , lANG_THAI            = LANG_THAI
 , lANG_URDU            = LANG_URDU
 , lANG_INDONESIAN      = LANG_INDONESIAN
 , lANG_BELARUSIAN      = LANG_BELARUSIAN
 , lANG_ESTONIAN        = LANG_ESTONIAN
 , lANG_LATVIAN         = LANG_LATVIAN
 , lANG_LITHUANIAN      = LANG_LITHUANIAN
 , lANG_FARSI           = LANG_FARSI
 , lANG_VIETNAMESE      = LANG_VIETNAMESE
 , lANG_ARMENIAN        = LANG_ARMENIAN
 , lANG_AZERI           = LANG_AZERI
 , lANG_BASQUE          = LANG_BASQUE
 , lANG_MACEDONIAN      = LANG_MACEDONIAN
 , lANG_AFRIKAANS       = LANG_AFRIKAANS
 , lANG_GEORGIAN        = LANG_GEORGIAN
 , lANG_FAEROESE        = LANG_FAEROESE
 , lANG_HINDI           = LANG_HINDI
 , lANG_MALAY           = LANG_MALAY
 , lANG_KAZAK           = LANG_KAZAK
 , lANG_SWAHILI         = LANG_SWAHILI
 , lANG_UZBEK           = LANG_UZBEK
 , lANG_TATAR           = LANG_TATAR
 , lANG_BENGALI         = LANG_BENGALI
 , lANG_PUNJABI         = LANG_PUNJABI
 , lANG_GUJARATI        = LANG_GUJARATI
 , lANG_ORIYA           = LANG_ORIYA
 , lANG_TAMIL           = LANG_TAMIL
 , lANG_TELUGU          = LANG_TELUGU
 , lANG_KANNADA         = LANG_KANNADA
 , lANG_MALAYALAM       = LANG_MALAYALAM
 , lANG_ASSAMESE        = LANG_ASSAMESE
 , lANG_MARATHI         = LANG_MARATHI
 , lANG_SANSKRIT        = LANG_SANSKRIT
 , lANG_KONKANI         = LANG_KONKANI
 , lANG_MANIPURI        = LANG_MANIPURI
 , lANG_SINDHI          = LANG_SINDHI
 , lANG_KASHMIRI        = LANG_KASHMIRI
 , lANG_NEPALI          = LANG_NEPALI
 }

#{enum SortID,
 , sORT_DEFAULT         = SORT_DEFAULT
 , sORT_JAPANESE_XJIS   = SORT_JAPANESE_XJIS
 , sORT_JAPANESE_UNICODE = SORT_JAPANESE_UNICODE
 , sORT_CHINESE_BIG5    = SORT_CHINESE_BIG5
 , sORT_CHINESE_UNICODE = SORT_CHINESE_UNICODE
 , sORT_KOREAN_KSC      = SORT_KOREAN_KSC
 , sORT_KOREAN_UNICODE  = SORT_KOREAN_UNICODE
 }

#{enum SubLANGID,
 , sUBLANG_NEUTRAL                      = SUBLANG_NEUTRAL
 , sUBLANG_DEFAULT                      = SUBLANG_DEFAULT
 , sUBLANG_SYS_DEFAULT                  = SUBLANG_SYS_DEFAULT
 , sUBLANG_CHINESE_TRADITIONAL          = SUBLANG_CHINESE_TRADITIONAL
 , sUBLANG_CHINESE_SIMPLIFIED           = SUBLANG_CHINESE_SIMPLIFIED
 , sUBLANG_CHINESE_HONGKONG             = SUBLANG_CHINESE_HONGKONG
 , sUBLANG_CHINESE_SINGAPORE            = SUBLANG_CHINESE_SINGAPORE
 , sUBLANG_DUTCH                        = SUBLANG_DUTCH
 , sUBLANG_DUTCH_BELGIAN                = SUBLANG_DUTCH_BELGIAN
 , sUBLANG_ENGLISH_US                   = SUBLANG_ENGLISH_US
 , sUBLANG_ENGLISH_UK                   = SUBLANG_ENGLISH_UK
 , sUBLANG_ENGLISH_AUS                  = SUBLANG_ENGLISH_AUS
 , sUBLANG_ENGLISH_CAN                  = SUBLANG_ENGLISH_CAN
 , sUBLANG_ENGLISH_NZ                   = SUBLANG_ENGLISH_NZ
 , sUBLANG_ENGLISH_EIRE                 = SUBLANG_ENGLISH_EIRE
 , sUBLANG_FRENCH                       = SUBLANG_FRENCH
 , sUBLANG_FRENCH_BELGIAN               = SUBLANG_FRENCH_BELGIAN
 , sUBLANG_FRENCH_CANADIAN              = SUBLANG_FRENCH_CANADIAN
 , sUBLANG_FRENCH_SWISS                 = SUBLANG_FRENCH_SWISS
 , sUBLANG_GERMAN                       = SUBLANG_GERMAN
 , sUBLANG_GERMAN_SWISS                 = SUBLANG_GERMAN_SWISS
 , sUBLANG_GERMAN_AUSTRIAN              = SUBLANG_GERMAN_AUSTRIAN
 , sUBLANG_ITALIAN                      = SUBLANG_ITALIAN
 , sUBLANG_ITALIAN_SWISS                = SUBLANG_ITALIAN_SWISS
 , sUBLANG_NORWEGIAN_BOKMAL             = SUBLANG_NORWEGIAN_BOKMAL
 , sUBLANG_NORWEGIAN_NYNORSK            = SUBLANG_NORWEGIAN_NYNORSK
 , sUBLANG_PORTUGUESE                   = SUBLANG_PORTUGUESE
 , sUBLANG_PORTUGUESE_BRAZILIAN         = SUBLANG_PORTUGUESE_BRAZILIAN
 , sUBLANG_SPANISH                      = SUBLANG_SPANISH
 , sUBLANG_SPANISH_MEXICAN              = SUBLANG_SPANISH_MEXICAN
 , sUBLANG_SPANISH_MODERN               = SUBLANG_SPANISH_MODERN
 , sUBLANG_ARABIC_SAUDI_ARABIA          = SUBLANG_ARABIC_SAUDI_ARABIA
 , sUBLANG_ARABIC_IRAQ                  = SUBLANG_ARABIC_IRAQ
 , sUBLANG_ARABIC_EGYPT                 = SUBLANG_ARABIC_EGYPT
 , sUBLANG_ARABIC_LIBYA                 = SUBLANG_ARABIC_LIBYA
 , sUBLANG_ARABIC_ALGERIA               = SUBLANG_ARABIC_ALGERIA
 , sUBLANG_ARABIC_MOROCCO               = SUBLANG_ARABIC_MOROCCO
 , sUBLANG_ARABIC_TUNISIA               = SUBLANG_ARABIC_TUNISIA
 , sUBLANG_ARABIC_OMAN                  = SUBLANG_ARABIC_OMAN
 , sUBLANG_ARABIC_YEMEN                 = SUBLANG_ARABIC_YEMEN
 , sUBLANG_ARABIC_SYRIA                 = SUBLANG_ARABIC_SYRIA
 , sUBLANG_ARABIC_JORDAN                = SUBLANG_ARABIC_JORDAN
 , sUBLANG_ARABIC_LEBANON               = SUBLANG_ARABIC_LEBANON
 , sUBLANG_ARABIC_KUWAIT                = SUBLANG_ARABIC_KUWAIT
 , sUBLANG_ARABIC_UAE                   = SUBLANG_ARABIC_UAE
 , sUBLANG_ARABIC_BAHRAIN               = SUBLANG_ARABIC_BAHRAIN
 , sUBLANG_ARABIC_QATAR                 = SUBLANG_ARABIC_QATAR
 , sUBLANG_AZERI_CYRILLIC               = SUBLANG_AZERI_CYRILLIC
 , sUBLANG_AZERI_LATIN                  = SUBLANG_AZERI_LATIN
 , sUBLANG_CHINESE_MACAU                = SUBLANG_CHINESE_MACAU
 , sUBLANG_ENGLISH_SOUTH_AFRICA         = SUBLANG_ENGLISH_SOUTH_AFRICA
 , sUBLANG_ENGLISH_JAMAICA              = SUBLANG_ENGLISH_JAMAICA
 , sUBLANG_ENGLISH_CARIBBEAN            = SUBLANG_ENGLISH_CARIBBEAN
 , sUBLANG_ENGLISH_BELIZE               = SUBLANG_ENGLISH_BELIZE
 , sUBLANG_ENGLISH_TRINIDAD             = SUBLANG_ENGLISH_TRINIDAD
 , sUBLANG_ENGLISH_PHILIPPINES          = SUBLANG_ENGLISH_PHILIPPINES
 , sUBLANG_ENGLISH_ZIMBABWE             = SUBLANG_ENGLISH_ZIMBABWE
 , sUBLANG_FRENCH_LUXEMBOURG            = SUBLANG_FRENCH_LUXEMBOURG
 , sUBLANG_FRENCH_MONACO                = SUBLANG_FRENCH_MONACO
 , sUBLANG_GERMAN_LUXEMBOURG            = SUBLANG_GERMAN_LUXEMBOURG
 , sUBLANG_GERMAN_LIECHTENSTEIN         = SUBLANG_GERMAN_LIECHTENSTEIN
 , sUBLANG_KASHMIRI_INDIA               = SUBLANG_KASHMIRI_INDIA
 , sUBLANG_KOREAN                       = SUBLANG_KOREAN
 , sUBLANG_LITHUANIAN                   = SUBLANG_LITHUANIAN
 , sUBLANG_MALAY_MALAYSIA               = SUBLANG_MALAY_MALAYSIA
 , sUBLANG_MALAY_BRUNEI_DARUSSALAM      = SUBLANG_MALAY_BRUNEI_DARUSSALAM
 , sUBLANG_NEPALI_INDIA                 = SUBLANG_NEPALI_INDIA
 , sUBLANG_SERBIAN_LATIN                = SUBLANG_SERBIAN_LATIN
 , sUBLANG_SERBIAN_CYRILLIC             = SUBLANG_SERBIAN_CYRILLIC
 , sUBLANG_SPANISH_GUATEMALA            = SUBLANG_SPANISH_GUATEMALA
 , sUBLANG_SPANISH_COSTA_RICA           = SUBLANG_SPANISH_COSTA_RICA
 , sUBLANG_SPANISH_PANAMA               = SUBLANG_SPANISH_PANAMA
 , sUBLANG_SPANISH_DOMINICAN_REPUBLIC   = SUBLANG_SPANISH_DOMINICAN_REPUBLIC
 , sUBLANG_SPANISH_VENEZUELA            = SUBLANG_SPANISH_VENEZUELA
 , sUBLANG_SPANISH_COLOMBIA             = SUBLANG_SPANISH_COLOMBIA
 , sUBLANG_SPANISH_PERU                 = SUBLANG_SPANISH_PERU
 , sUBLANG_SPANISH_ARGENTINA            = SUBLANG_SPANISH_ARGENTINA
 , sUBLANG_SPANISH_ECUADOR              = SUBLANG_SPANISH_ECUADOR
 , sUBLANG_SPANISH_CHILE                = SUBLANG_SPANISH_CHILE
 , sUBLANG_SPANISH_URUGUAY              = SUBLANG_SPANISH_URUGUAY
 , sUBLANG_SPANISH_PARAGUAY             = SUBLANG_SPANISH_PARAGUAY
 , sUBLANG_SPANISH_BOLIVIA              = SUBLANG_SPANISH_BOLIVIA
 , sUBLANG_SPANISH_EL_SALVADOR          = SUBLANG_SPANISH_EL_SALVADOR
 , sUBLANG_SPANISH_HONDURAS             = SUBLANG_SPANISH_HONDURAS
 , sUBLANG_SPANISH_NICARAGUA            = SUBLANG_SPANISH_NICARAGUA
 , sUBLANG_SPANISH_PUERTO_RICO          = SUBLANG_SPANISH_PUERTO_RICO
 , sUBLANG_SWEDISH                      = SUBLANG_SWEDISH
 , sUBLANG_SWEDISH_FINLAND              = SUBLANG_SWEDISH_FINLAND
 , sUBLANG_URDU_PAKISTAN                = SUBLANG_URDU_PAKISTAN
 , sUBLANG_URDU_INDIA                   = SUBLANG_URDU_INDIA
 , sUBLANG_UZBEK_LATIN                  = SUBLANG_UZBEK_LATIN
 , sUBLANG_UZBEK_CYRILLIC               = SUBLANG_UZBEK_CYRILLIC
 }

-- , SUBLANG_LITHUANIAN_CLASSIC (not in mingw-20001111)

-- ----------------------------------------------------------------------------

-- | The `System.IO` input functions (e.g. `getLine`) don't
-- automatically convert to Unicode, so this function is provided to
-- make the conversion from a multibyte string in the given code page
-- to a proper Unicode string.  To get the code page for the console,
-- use `getConsoleCP`.

stringToUnicode :: CodePage -> String -> IO String
stringToUnicode _cp "" = return ""
     -- MultiByteToWideChar doesn't handle empty strings (#1929)
stringToUnicode cp mbstr =
  withCAStringLen mbstr $ \(cstr,len) -> do
    wchars <- failIfZero "MultiByteToWideChar" $ multiByteToWideChar
                cp
                0
                cstr
                (fromIntegral len)
                nullPtr 0
    -- wchars is the length of buffer required
    allocaArray (fromIntegral wchars) $ \cwstr -> do
      wchars' <- failIfZero "MultiByteToWideChar" $ multiByteToWideChar
                cp
                0
                cstr
                (fromIntegral len)
                cwstr wchars
      peekCWStringLen (cwstr,fromIntegral wchars')  -- converts UTF-16 to [Char]

foreign import WINDOWS_CCONV unsafe "MultiByteToWideChar"
  multiByteToWideChar
        :: CodePage
        -> DWORD   -- dwFlags,
        -> LPCSTR  -- lpMultiByteStr
        -> CInt    -- cbMultiByte
        -> LPWSTR  -- lpWideCharStr
        -> CInt    -- cchWideChar
        -> IO CInt
