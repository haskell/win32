/* The version of wincon.h provided by the version of MSYS2 included with x86
 * versions of GHC before GHC 7.10 excludes certain components introduced with
 * Windows Vista.
 */

#ifndef WINCON_COMPAT_H
#define WINCON_COMPAT_H

#if defined(x86_64_HOST_ARCH) || __GLASGOW_HASKELL__ > 708
#
#else

typedef struct _CONSOLE_SCREEN_BUFFER_INFOEX {
  ULONG      cbSize;
  COORD      dwSize;
  COORD      dwCursorPosition;
  WORD       wAttributes;
  SMALL_RECT srWindow;
  COORD      dwMaximumWindowSize;
  WORD       wPopupAttributes;
  WINBOOL    bFullscreenSupported;
  COLORREF   ColorTable[16];
} CONSOLE_SCREEN_BUFFER_INFOEX, *PCONSOLE_SCREEN_BUFFER_INFOEX;

#endif /* GHC version check */
#endif /* WINCON_COMPAT_H */
