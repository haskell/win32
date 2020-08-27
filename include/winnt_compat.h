/* The version of winnt.h provided by the version of MSYS2 included with
 * versions of GHC before GHC 7.10 excludes certain components introduced with
 * Windows Vista.
 */

#ifndef WINNT_COMPAT_H
#define WINNT_COMPAT_H

#if __GLASGOW_HASKELL__ < 710 && defined(i386_HOST_ARCH)
#define LOCALE_NAME_MAX_LENGTH 85
#endif

#endif /* #ifndef WINNT_COMPAT_H */
