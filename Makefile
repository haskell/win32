# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.2 2003/05/17 00:53:51 ross Exp $

TOP = ..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------

# Comment out if you want to do initial debugging on Unix systems
SUBDIRS = cbits

ALL_DIRS = \
	System \
	System/Win32 

PACKAGE = Win32
PACKAGE_DEPS = base

SRC_CC_OPTS += -Wall -I../include -I.
SRC_CC_OPTS += -I$(GHC_INCLUDE_DIR) -I$(GHC_RUNTIME_DIR)

SRC_HC_OPTS += -Wall -fffi -cpp -fglasgow-exts -package lang
GC_OPTS += --target=ffi 

SRC_HADDOCK_OPTS += -t "Win32 Libraries (Win32 package)"

# yeuch, have to get Win32_CFLAGS & Win32_LIBS in through CPP to package.conf.in
comma = ,
PACKAGE_CPP_OPTS += -DWIN32_CFLAGS='$(patsubst %,$(comma)"%",$(WIN32_CFLAGS))'
PACKAGE_CPP_OPTS += -DWIN32_LIBS='$(patsubst %,$(comma)"%",$(WIN32_LIBS))'

# -----------------------------------------------------------------------------

# Comment out if you want to do initial debugging on Unix systems
STUBOBJS += \
   $(patsubst %.gc,  %_stub_ffi.o, $(GC_SRCS))

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
