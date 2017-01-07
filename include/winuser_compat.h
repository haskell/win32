#ifndef WINUSER_COMPAT_H
#define WINUSER_COMPAT_H

/*
 * winuser.h is missing some includes in MinGW, which was shipped with the 32-bit
 * Windows version of GHC prior to the 7.10.3 release.
 */
#if defined(x86_64_HOST_ARCH) || \
   __GLASGOW_HASKELL__ >= 711 || \
   (__GLASGOW_HASKELL__ == 710 && \
    defined(__GLASGOW_HASKELL_PATCHLEVEL1__) && \
    __GLASGOW_HASKELL_PATCHLEVEL1__ >= 2)
#else
// Some declarations from winuser.h that we need in Win32
# include <windows.h>

#define VK_XBUTTON1 0x05
#define VK_XBUTTON2 0x06

#define VK_BROWSER_BACK 0xA6
#define VK_BROWSER_FORWARD 0xA7
#define VK_BROWSER_REFRESH 0xA8
#define VK_BROWSER_STOP 0xA9
#define VK_BROWSER_SEARCH 0xAA
#define VK_BROWSER_FAVORITES 0xAB
#define VK_BROWSER_HOME 0xAC
#define VK_VOLUME_MUTE 0xAD
#define VK_VOLUME_DOWN 0xAE
#define VK_VOLUME_UP 0xAF
#define VK_MEDIA_NEXT_TRACK 0xB0
#define VK_MEDIA_PREV_TRACK 0xB1
#define VK_MEDIA_STOP 0xB2
#define VK_MEDIA_PLAY_PAUSE 0xB3
#define VK_LAUNCH_MAIL 0xB4
#define VK_LAUNCH_MEDIA_SELECT 0xB5
#define VK_LAUNCH_APP1 0xB6
#define VK_LAUNCH_APP2 0xB7

#define VK_OEM_PLUS 0xBB
#define VK_OEM_COMMA 0xBC
#define VK_OEM_MINUS 0xBD
#define VK_OEM_PERIOD 0xBE
#define VK_OEM_102 0xE2

#define VK_PACKET 0xE7

#define LWA_COLORKEY 0x00000001
#define LWA_ALPHA 0x00000002

#define ULW_COLORKEY 0x00000001
#define ULW_ALPHA 0x00000002
#define ULW_OPAQUE 0x00000004
#define ULW_EX_NORESIZE 0x00000008

#endif /* GHC Version check */
#endif /* WINUSER_COMPAT_H */
