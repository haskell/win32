#ifndef TLHELP32_COMPAT_H
#define TLHELP32_COMPAT_H
 /*
 * tlhelp32.h is not included in MinGW, which was shipped with the 32-bit
 * Windows version of GHC prior to the 7.10.3 release.
 */
#if __GLASGOW_HASKELL__ > 708
#else
// Some declarations from tlhelp32.h that we need in Win32
#include <windows.h>

#define TH32CS_SNAPMODULE32 0x00000010

#endif
#endif /* TLHELP32_COMPAT_H */
