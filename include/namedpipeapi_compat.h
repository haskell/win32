/* The version of wincon.h provided by the version of MSYS2 included with x86
 * versions of GHC before GHC 7.10 excludes certain components introduced with
 * Windows Vista.
 */

#ifndef NAMEDPIPEAPI_COMPAT_H
#define NAMEDPIPEAPI_COMPAT_H

#if defined(x86_64_HOST_ARCH) || __GLASGOW_HASKELL__ > 708
#
#else

#define PIPE_ACCEPT_REMOTE_CLIENTS 0x0
#define PIPE_REJECT_REMOTE_CLIENTS 0x8
#endif /* GHC version check */
#endif /* NAMEDPIPEAPI_COMPAT_H */
