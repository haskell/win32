AC_INIT([Haskell Win32 package], [1.1], [libraries@haskell.org], [Win32])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([include/HsWin32.h])
AC_CONFIG_HEADERS([include/HsWin32Config.h])

AC_ARG_WITH([unicows-dll],
[AC_HELP_STRING([--with-unicows-dll=ARG],
                [Use ARG as the path to your local unicows DLL [default=autodetect]])],
[UnicowsDLL=$withval],[UnicowsDLL=`type -p unicows.dll`])
AC_SUBST(UnicowsDLL)

AC_ARG_WITH([libunicows-dir],
[AC_HELP_STRING([--with-libunicows-dir=ARG],
                [Use ARG as the directory path to your local unicows import library [default=none]])],
[LibUnicowsDir=$withval],[LibUnicowsDir=""])
AC_SUBST(LibUnicowsDir)

# Just for fun..
AC_CHECK_HEADERS([windows.h])

if test "$LibUnicowsDir" != ""; then
  save_libs="$LIBS"
  LIBS="-L$LibUnicowsDir $LIBS"
fi
AC_CHECK_LIB(unicows,LoadUnicowsSymbol,[fp_unicows_found=yes],[fp_unicows_found=no])
if test "$LibUnicowsDir" != ""; then
  LIBS="$save_libs"
fi

if test "$fp_unicows_found" != "yes"; then
   # OK, let's give the user a bit of a fright by dumping out this long warning msg...
   echo "WARNING: couldn't locate the 'libunicows.a' import library -- "
   echo ""
   echo "         The 'libunicows' helper library is a required component for Haskell applications"
   echo "         that use the Win32 package and wish to operate correctly on Windows 9x"
   echo "         installations. If that's a concern, you can get it from: "
   echo ""
   echo "              http://libunicows.sourceforge.net/"
   echo ""
   echo "         That page hosts the mingw32-friendly import libraries for interfacing to the"
   echo "         actual DLLs that implement the Win32 Unicode compatibility layer."
   echo "         Links to both Microsoft's MSLU and Mozilla's Opencow/MZLU DLLs are provided via"
   echo "         the above page (along with background technical information as to why this is"
   echo "         needed in the first place.) You'll need to download and install one (or both) also."
   echo ""
   echo "Note: The GHC compiler itself depends on the Win32 package, and hence re-distributes"
   echo "      copies of the unicows stub library (for mingw32) together with Mozilla's"
   echo "      opencow.dll (renamed as unicows.dll in its bin/ directory). This is with"
   echo "      ghc-6.6.1 and later."
   AC_DEFINE(UNICOWSLIB,unicows,[Name of unicows import library])
   HaveUnicows=NO
else
   HaveUnicows=YES
fi
AC_SUBST(HaveUnicows)

AC_CONFIG_FILES([config.mk])
AC_OUTPUT
