# Changelog for [`Win32` package](http://hackage.haskell.org/package/Win32)

## 2.3.1.1 *May 2016*

* Release for GHC 8.0.1

## GIT HEAD (Unknown version)

* Add `windows_cconv.h` to the `install-includes` field of `Win32.cabal`,
  allowing packages that transitively depend on `Win32` to use the
  `WINDOWS_CCONV` CPP macro (which expands to `stdcall` or `ccall`
  appropriately depending on the system architecture)
* Added function `getLongPathName`
* Added function `getShortPathName`
* Added function `getUserName`
* Added file attribute `fILE_ATTRIBUTE_REPARSE_POINT`
* Added more [`File Access Rights` constants](https://msdn.microsoft.com/en-us/library/windows/desktop/gg258116%28v=vs.85%29.aspx)
