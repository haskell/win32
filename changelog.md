# Changelog for [`Win32` package](http://hackage.haskell.org/package/Win32)

## 2.9.1.0 August 2020

-   Add function isWindowVisible
-   Add function getLastInputInfo
-   Add function getTickCount
-   Add function getIdleTime

## 2.9.0.0 June 2020

-   `setWindowClosure` now returns the old window closure.
-   `defWindowProc` now assumes the data stored in `GWLP\_USERDATA`
    is the window closure (in line with `setWindowClosure` and
    the supplied C `genericWndProc`)
-   `defWindowProc` now frees the window closure
-   `getMessage` and `peekMessage` test for -1 to identify the error condition
-   Support creating symbolic links without Administrator privilege (See #147)
-   Support for `winio` the new Windows I/O manager.

## 2.8.5.0 Dec 2019

-   Add `getConsoleMode` and `setConsoleMode` (See #137)

## 2.8.4.0 _Oct 2019_

-   Added function `getWindowText`
-   Added function `getWindowTextLength`

## 2.8.3.0 _Feb 2019_

-   Add `Module32FirstW` and `Module32NextW` (See #121)
-   Add `Virtual[Alloc/Free]Ex` (See #124)

## 2.8.2.0 _Dec 2018_

-   Drop use of NegativeLiterals (See #118)

## 2.8.1.0 _Nov 2018_

-   Fix broken links (See #116)
-   Remove unused CPP Lower bounds (See #114)
-   GHC 8.8 release

## 2.8.0.0 _May 2018_

-   Deprecated `regQueryValueKey`. (See #105, #108)
-   Updated `regQueryValue` signature (See #108)
-   Add `regQueryDefaultValue` (See #108)
-   Add `regGetValue` and `RegTypeRestriction` (See #109)
-   Remove `sYNCHRONIZE` from System.Win32.Process, use System.Win32.File instead. (See #110)

## 2.7.1.0 _April 2018_

-   Fixed `MOUSEINPUT` storable instance. (See #106)

## 2.7.0.0 _March 2018_

-   Fixed `DWORD_PTR` type (See #99)
-   Add `lockFile` and `unlockFile` (See #103)

## 2.6.2.0 _December 2017_

-   Add `setFilePointerEx` (See #94)
-   Add `getConsoleScreenBufferInfo` and `getCurrentConsoleScreenBufferInfo` (See #95)

## 2.6.1.0 _November 2017_

-   Add `terminateProcessById` (See #91)

## 2.6.0.0 _September 2017_

-   Make cabal error out on compilation on non-Windows OSes. (See #80)
-   Update cabal format to 1.10 and set language
    default to Haskell2010. (See #81)
-   Use `Maybe` in wrappers for functions with nullable pointer parameters (See #83)
-   Improve cross compilation support. (See #87)

## 2.5.4.1 _April 2017_

-   Fixed GetWindowLong on 32-bit Windows

## 2.5.3.0 _March 2017_

-   Fix buffer overflow in `regSetValue`. (See #39)
-   Added `getPixel`. (See #37)
-   Drop dependency on `ntdll` because of incorrect import library on x86. (See #79)

## 2.5.2.0 _March 2017_

-   Fix constant underflows with (-1) and unsigned numbers.
-   Add `commandLineToArgv`

## 2.5.1.0 _Feb 2017_

-   Add `withHandleToHANDLE` (originally found in the `ansi-terminal` library)
-   fixed `PokeTZI` test

## 2.5.0.0 _Jan 2017_

-   `failWith` (and the API calls that use it) now throw `IOError`s with proper
    `IOErrorType`s.
-   Add function `findWindowByName`
-   Fix a bug in the implementation of `poke` for `TIME_ZONE_INFORMATION` which
    would cause it to be marshalled incorrectly.
-   Add `System.Win32.MinTTY` module for detecting the presence of MinTTY.
-   Add `ULONG` type to `System.Win32.Types`.
-   Add function `failIfNeg` to `System.Win32.Types`, which fails if a negative
    number is returned. This simulates the behavior of the `NT_SUCCESS` macro.
-   Merged package Win32-extras (See #16)
-   `Graphics.Win32.Misc.messageBox` safely imported now https://github.com/haskell/win32/pull/5
-   Fixed various alignment calls that were incorrect. These would result in an incorrect alignment
    being returned on certain platforms. (See #66)

## 2.4.0.0 _Nov 2016_

-   Add `windows_cconv.h` to the `install-includes` field of `Win32.cabal`,
    allowing packages that transitively depend on `Win32` to use the
    `WINDOWS_CCONV` CPP macro (which expands to `stdcall` or `ccall`
    appropriately depending on the system architecture)
-   Added function `getLongPathName`
-   Added function `getShortPathName`
-   Added function `getUserName`
-   Added file attribute `fILE_ATTRIBUTE_REPARSE_POINT`
-   Added more [`File Access Rights` constants](https://msdn.microsoft.com/en-us/library/windows/desktop/gg258116%28v=vs.85%29.aspx)
-   Added function `getCurrentProcessId`
-   Added function `filepathRelativePathTo`
-   Added function `pathRelativePathTo`
-   Corrected 64 bit types (See #53)

## 2.3.1.1 _May 2016_

-   Release for GHC 8.0.1
