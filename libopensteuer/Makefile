# ------------------------------------------------------------------
# LibOpenSteuer - An open-source taxlibrary based on german tax laws
# ------------------------------------------------------------------

# This is the main Makefile. Start compiling from here.

# ---------------------------------------------------------------------
# Edit these paths, if you want your installation in a different place:
# ---------------------------------------------------------------------

# PREFIX is the main path to the actual subdirs:
PREFIX = /usr/local

# LIB_DIR is the path to the directory where the library goes:
export LIB_DIR = $(PREFIX)/lib

# INCLUDE_DIR is the path to the directory where the header go:
export INCLUDE_DIR = $(PREFIX)/include/libopensteuer

# MAN3_DIR is the path to the directory, where the man(3)-pages go:
export MAN3_DIR = $(PREFIX)/man/man3

# ------------------------------------------------------------
# Programs we need for the installation. Make sure they exist:
# ------------------------------------------------------------

export SHELL = /bin/sh
export INSTALL = /usr/bin/install
export RMDIR = /bin/rmdir

# for shared libs:
export LDCONFIG = /sbin/ldconfig
export LN = /bin/ln
# for static libs:
export RANLIB = /usr/bin/ranlib

# --------------------------------------------------------------
# The API-version. Only change it if you know what you're doing:
# --------------------------------------------------------------

export API = 0

# -------------------------------
# For internal use. Don't change:
# -------------------------------

ADA_DIR = ./src/ada
C_DIR = ./src/c
DOC_DIR = ./Documentation

export VERSION = $(shell cat VERSION)

.PHONY: all debug devel
all debug devel:
	@cd $(ADA_DIR) && $(MAKE) $@

.PHONY: shared static
shared static:
	@cd $(ADA_DIR) && $(MAKE) $@

.PHONY: shared_install static_install shared_devel_install
shared_install static_install shared_devel_install:
	@cd $(ADA_DIR) && $(MAKE) $@
	@cd $(DOC_DIR) && $(MAKE) man_install

.PHONY: uninstall
uninstall:
	@cd $(ADA_DIR) && $(MAKE) $@
	@cd $(C_DIR) && $(MAKE) $@
	@cd $(DOC_DIR) && $(MAKE) $@

.PHONY: clean
clean:
	@cd $(ADA_DIR) && $(MAKE) $@
	@cd $(C_DIR) && $(MAKE) $@
	@cd $(DOC_DIR) && $(MAKE) $@
	$(RM) *~
