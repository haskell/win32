# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.11 2004/05/05 14:59:02 ross Exp $

TOP = ..
include $(TOP)/mk/boilerplate.mk

# include mk/version.mk

# -----------------------------------------------------------------------------

# Comment out if you want to do initial debugging on Unix systems
SUBDIRS = cbits include

ALL_DIRS = \
	Graphics \
	Graphics/Win32 \
	Graphics/Win32/GDI \
	System \
	System/Win32 

PACKAGE = Win32
PACKAGE_DEPS = base

SRC_HSC2HS_OPTS += -Iinclude
SRC_CC_OPTS += -Iinclude
SRC_HC_OPTS += -Wall 
SRC_HC_OPTS += -optc-Iinclude -Iinclude
SRC_HC_OPTS += -fffi -O

SRC_HADDOCK_OPTS += -t "Win32 Libraries ($(PACKAGE) package)"

# _stub.o files are a side-effect from compiling .hs files that
# contain 'foreign export' declarations.
EXTRA_C_OBJS += Graphics/Win32/Dialogue_stub.o Graphics/Win32/Window_stub.o

STUBOBJS    += $(filter-out $(EXTRA_C_OBJS), $(patsubst %.c, %.o, $(C_SRCS))) $(EXTRA_C_OBJS)

Graphics/Win32/Dialogue_stub.o : Graphics/Win32/Dialogue.o
	@:
Graphics/Win32/Window_stub.o : Graphics/Win32/Window.o
	@:

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk
