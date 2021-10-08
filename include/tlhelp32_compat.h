#ifndef TLHELP32_COMPAT_H
#define TLHELP32_COMPAT_H
 /*
 * tlhelp32.h is not included in MinGW, which was shipped with the 32-bit
 * Windows version of GHC prior to the 7.10.3 release.
 */
#if __GLASGOW_HASKELL__ > 708
#else
// Declarations from tlhelp32.h that Win32 requires
#include <windows.h>

// CreateToolhelp32Snapshot Flags
// https://docs.microsoft.com/en-us/windows/win32/api/tlhelp32/nf-tlhelp32-createtoolhelp32snapshot

#define TH32CS_INHERIT      0x80000000

#define TH32CS_SNAPHEAPLIST 0x00000001
#define TH32CS_SNAPPROCESS  0x00000002
#define TH32CS_SNAPTHREAD   0x00000004
#define TH32CS_SNAPMODULE   0x00000008
#define TH32CS_SNAPMODULE32 0x00000010

#define TH32CS_SNAPALL (TH32CS_SNAPHEAPLIST|TH32CS_SNAPPROCESS|TH32CS_SNAPTHREAD|TH32CS_SNAPMODULE)

#endif
#endif /* TLHELP32_COMPAT_H */
