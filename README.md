The `Win32` Package
=====================

[![Hackage](https://img.shields.io/hackage/v/Win32.svg)](https://hackage.haskell.org/package/Win32) [![PyPI](https://img.shields.io/pypi/l/Django.svg)]() [![Windows build status](https://ci.appveyor.com/api/projects/status/github/haskell/win32?branch=master&svg=true)](https://ci.appveyor.com/project/hvr/win32)

See [`Win32` on Hackage](http://hackage.haskell.org/package/Win32) for
more information.

Installing from Git
-------------------

This package requires no special installation instructions.

To install use `cabal install`

Documentation
-------------------
This library is just a direct binding to Windows API calls and
as such contains no documentation. The documentation for functions
can be found in the equivalently named functions on MSDN 
https://msdn.microsoft.com/library/windows/desktop/hh920508.aspx

Getting Started
---------------
The `Win32` library is a core GHC library and as such aims to have an
as little as possible footprint when it comes to dependencies.

When submitting new requests think hard if any new `Haskell` dependencies are
actually needed. (note that this does not apply to `C` dependencies.)

The best way to get started is using `cabal new-build` and `cabal sandboxes`:

```
git clone git@github.com:haskell/win32.git
cd win32
cabal sandbox init
cabal install
cabal repl
```

Testsuite
---------
The tests in the `Win32` package are designed to run as part of the GHC testsuite.
As such, should you want to add a new test, you will need the testsuite drivers.

See https://ghc.haskell.org/trac/ghc/wiki/Building/RunningTests/Adding for details.
