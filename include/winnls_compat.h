/* The version of winnls.h provided by the version of MSYS2 included with
 * versions of GHC before GHC 7.10 excludes certain components introduced with
 * Windows Vista.
 */

#ifndef WINNLS_COMPAT_H
#define WINNLS_COMPAT_H

#if __GLASGOW_HASKELL__ < 710
// Locale information constants
#define LOCALE_IGEOID 0x0000005b
#define LOCALE_SCONSOLEFALLBACKNAME 0x0000006e
#define LOCALE_SDURATION 0x0000005d
#define LOCALE_SENGLISHCOUNTRYNAME 0x00001002
#define LOCALE_SENGLISHLANGUAGENAME 0x00001001
#define LOCALE_SISO3166CTRYNAME2 0x00000068
#define LOCALE_SISO639LANGNAME2 0x00000067
#define LOCALE_SKEYBOARDSTOINSTALL 0x0000005e
#define LOCALE_SNAME 0x0000005c
#define LOCALE_SNAN 0x00000069
#define LOCALE_SNATIVECOUNTRYNAME 0x00000008
#define LOCALE_SNEGINFINITY 0x0000006b
#define LOCALE_SPARENT 0x0000006d
#define LOCALE_SPOSINFINITY 0x0000006a
#define LOCALE_SSCRIPTS 0x0000006c
#define LOCALE_SSHORTESTDAYNAME1 0x00000060
#define LOCALE_SSHORTESTDAYNAME2 0x00000061
#define LOCALE_SSHORTESTDAYNAME3 0x00000062
#define LOCALE_SSHORTESTDAYNAME4 0x00000063
#define LOCALE_SSHORTESTDAYNAME5 0x00000064
#define LOCALE_SSHORTESTDAYNAME6 0x00000065
#define LOCALE_SSHORTESTDAYNAME7 0x00000066
// Locale map flag constants
#define LINGUISTIC_IGNORECASE 0x00000010
#define LINGUISTIC_IGNOREDIACRITIC 0x00000020
#define NORM_LINGUISTIC_CASING 0x08000000
// Other
WINBASEAPI WINBOOL WINAPI IsValidLocaleName (LPCWSTR lpLocaleName);
#endif

#if __GLASGOW_HASKELL__ < 710 && defined(i386_HOST_ARCH)
// Locale information constants
#define LOCALE_IDEFAULTMACCODEPAGE 0x00001011
// Other
typedef struct _nlsversioninfoex {
  DWORD dwNLSVersionInfoSize;
  DWORD dwNLSVersion;
  DWORD dwDefinedVersion;
  DWORD dwEffectiveId;
  GUID  guidCustomVersion;
} NLSVERSIONINFOEX, *LPNLSVERSIONINFOEX;

WINBASEAPI int WINAPI GetLocaleInfoEx(
  LPCWSTR lpLocaleName,
  LCTYPE LCType,
  LPWSTR lpLCData,
  int cchData
);

WINBASEAPI WINBOOL WINAPI GetNLSVersionEx(
  NLS_FUNCTION function,
  LPCWSTR lpLocaleName,
  LPNLSVERSIONINFOEX lpVersionInformation
);

WINBASEAPI int WINAPI LCMapStringEx(
  LPCWSTR lpLocaleName,
  DWORD dwMapFlags,
  LPCWSTR lpSrcStr,
  int cchSrc,
  LPWSTR lpDestStr,
  int cchDest,
  LPNLSVERSIONINFO lpVersionInformation,
  LPVOID lpReserved,
  LPARAM lParam
);


WINBASEAPI int WINAPI GetTimeFormatEx(
  LPCWSTR lpLocaleName,
  DWORD dwFlags,
  const SYSTEMTIME *lpTime,
  LPCWSTR lpFormat,
  LPWSTR lpTimeStr,
  int cchTime
);

WINBASEAPI int WINAPI GetSystemDefaultLocaleName(
  LPWSTR lpLocaleName,
  int cchLocaleName
);

WINBASEAPI int WINAPI GetUserDefaultLocaleName(
  LPWSTR lpLocaleName,
  int cchLocaleName
);
#endif

#endif /* #ifndef WINNLS_COMPAT_H */
