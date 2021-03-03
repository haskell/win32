# Changelog for [`Win32` package](http://hackage.haskell.org/package/Win32)

## *Unreleased*

* Fix Alignment of LASTINPUTINFO (See #172)

## 2.11.1.0 February 2021

* Win32 for GHC 9.2.x
* Make `System.Win32.NLS` re-export `CodePage` from `GHC.IO.Encoding.CodePage`
  in `base` when compiled with `base-4.15` or later.

## 2.11.0.0 January 2021

* Remove function `mapFileBs`.

## 2.10.1.0 October 2020

* Add `System.Win32.Event` module
* Add function `openEvent`
* Add function `createEvent`
* Add function `duplicateHandle`
* Add function `setEvent`
* Add function `resetEvent`
* Add function `pulseEvent`
* Add function `signalObjectAndWait`
* Add function `waitForSingleObject`
* Add function `waitForSingleObjectEx`
* Add function `waitForMultipleObjects`
* Add function `waitForMultipleObjectsEx`
* Add enums `DUPLICATE_CLOSE_SOURCE`, `DUPLICATE_SAME_ACCESS`,
  `EVENT_ALL_ACCESS`, `EVENT_MODIFY_STATE`, `WAIT_ABANDONED`,
  `WAIT_IO_COMPLETION`, `WAIT_OBJECT_0`, `WAIT_TIMEOUT` and `WAIT_FAILED`.
* Add struct `SECURITY_ATTRIBUTES`

## 2.10.0.0 September 2020

* Add function `isWindowVisible`
* Add function `getLastInputInfo`
* Add function `getTickCount`
* Add function `getIdleTime`
* Add `enumSystemLocalesEx`, `enumSystemLocalesEx'`,
  `getSystemDefaultLocaleName`, `getUserDefaultLocaleName`, `isValidLocaleName`,
  `getLocaleInfoEx`, `getTimeFormatEx` and `lCMapStringEx`
* Add `trySized` - similar to `try` but for API calls that return the required
  size of the buffer when passed a buffer size of zero.
* Add `fromDateFormatPciture` and `fromTimeFormatPicture`, to translate from
  Windows date and time format pictures to format strings used by the `time`
  package.
* Renamed fields of `COORD` and `SMALL_RECT` to avoid name clashes. (See #157)

## 2.9.0.0 June 2020

* `setWindowClosure` now returns the old window closure.
* `defWindowProc` now assumes the data stored in `GWLP\_USERDATA`
  is the window closure (in line with `setWindowClosure` and
  the supplied C `genericWndProc`)
* `defWindowProc` now frees the window closure
* `getMessage` and `peekMessage` test for -1 to identify the error condition
* Support creating symbolic links without Administrator privilege (See #147)
* Support for `winio` the new Windows I/O manager.

## 2.8.5.0 Dec 2019

* Add `getConsoleMode` and `setConsoleMode` (See #137)

## 2.8.4.0 *Oct 2019*

* Added function `getWindowText`
* Added function `getWindowTextLength`

## 2.8.3.0 *Feb 2019*

* Add `Module32FirstW` and `Module32NextW` (See #121)
* Add `Virtual[Alloc/Free]Ex` (See #124)

## 2.8.2.0 *Dec 2018*

* Drop use of NegativeLiterals (See #118)

## 2.8.1.0 *Nov 2018*

* Fix broken links (See #116)
* Remove unused CPP Lower bounds (See #114)
* GHC 8.8 release

## 2.8.0.0 *May 2018*

* Deprecated `regQueryValueKey`. (See #105, #108)
* Updated `regQueryValue` signature (See #108)
* Add `regQueryDefaultValue` (See #108)
* Add `regGetValue` and `RegTypeRestriction` (See #109)
* Remove `sYNCHRONIZE` from System.Win32.Process, use System.Win32.File instead. (See #110)

## 2.7.1.0 *April 2018*

* Fixed `MOUSEINPUT` storable instance. (See #106)

## 2.7.0.0 *March 2018*

* Fixed `DWORD_PTR` type (See #99)
* Add `lockFile` and `unlockFile` (See #103)

## 2.6.2.0 *December 2017*

* Add `setFilePointerEx` (See #94)
* Add `getConsoleScreenBufferInfo` and `getCurrentConsoleScreenBufferInfo` (See #95)

## 2.6.1.0 *November 2017*

* Add `terminateProcessById` (See #91)

## 2.6.0.0 *September 2017*

* Make cabal error out on compilation on non-Windows OSes. (See #80)
* Update cabal format to 1.10 and set language
  default to Haskell2010. (See #81)
* Use `Maybe` in wrappers for functions with nullable pointer parameters (See #83)
* Improve cross compilation support. (See #87)

## 2.5.4.1 *April 2017*

* Fixed GetWindowLong on 32-bit Windows

## 2.5.3.0 *March 2017*

* Fix buffer overflow in `regSetValue`. (See #39)
* Added `getPixel`. (See #37)
* Drop dependency on `ntdll` because of incorrect import library on x86. (See #79)

## 2.5.2.0 *March 2017*

* Fix constant underflows with (-1) and unsigned numbers.
* Add `commandLineToArgv`

## 2.5.1.0 *Feb 2017*

* Add `withHandleToHANDLE` (originally found in the `ansi-terminal` library)
* fixed `PokeTZI` test

## 2.5.0.0 *Jan 2017*

* `failWith` (and the API calls that use it) now throw `IOError`s with proper
  `IOErrorType`s.
* Add function `findWindowByName`
* Fix a bug in the implementation of `poke` for `TIME_ZONE_INFORMATION` which
  would cause it to be marshalled incorrectly.
* Add `System.Win32.MinTTY` module for detecting the presence of MinTTY.
* Add `ULONG` type to `System.Win32.Types`.
* Add function `failIfNeg` to `System.Win32.Types`, which fails if a negative
  number is returned. This simulates the behavior of the `NT_SUCCESS` macro.
* Merged package Win32-extras (See #16)
* `Graphics.Win32.Misc.messageBox` safely imported now https://github.com/haskell/win32/pull/5
* Fixed various alignment calls that were incorrect. These would result in an incorrect alignment
  being returned on certain platforms. (See #66)

## 2.4.0.0 *Nov 2016*

* Add `windows_cconv.h` to the `install-includes` field of `Win32.cabal`,
  allowing packages that transitively depend on `Win32` to use the
  `WINDOWS_CCONV` CPP macro (which expands to `stdcall` or `ccall`
  appropriately depending on the system architecture)
* Added function `getLongPathName`
* Added function `getShortPathName`
* Added function `getUserName`
* Added file attribute `fILE_ATTRIBUTE_REPARSE_POINT`
* Added more [`File Access Rights` constants](https://msdn.microsoft.com/en-us/library/windows/desktop/gg258116%28v=vs.85%29.aspx)
* Added function `getCurrentProcessId`
* Added function `filepathRelativePathTo`
* Added function `pathRelativePathTo`
* Corrected 64 bit types (See #53)

## 2.3.1.1 *May 2016*

* Release for GHC 8.0.1
