#ifndef WINCON_COMPAT_H
#define WINCON_COMPAT_H

/*
 * wincon.h is not included in MinGW, which was shipped with the 32-bit
 * Windows version of GHC prior to the 7.10.3 release.
 */

#if defined(x86_64_HOST_ARCH) || \
   __GLASGOW_HASKELL__ >= 711 || \
   (__GLASGOW_HASKELL__ == 710 && \
    defined(__GLASGOW_HASKELL_PATCHLEVEL1__) && \
    __GLASGOW_HASKELL_PATCHLEVEL1__ >= 2)

#else

# include <windows.h>

typedef struct _COORD {
    SHORT X;
    SHORT Y;
} COORD, *PCOORD;

typedef struct _SMALL_RECT {
    SHORT Left;
    SHORT Top;
    SHORT Right;
    SHORT Bottom;
} SMALL_RECT,*PSMALL_RECT;

typedef struct _CONSOLE_SCREEN_BUFFER_INFO {
    COORD    dwSize;
    COORD    dwCursorPosition;
    WORD     wAttributes;
    SMALL_RECT srWindow;
    COORD    dwMaximumWindowSize;
} CONSOLE_SCREEN_BUFFER_INFO,*PCONSOLE_SCREEN_BUFFER_INFO;

#endif

#endif /* WINCON_COMPAT_H */