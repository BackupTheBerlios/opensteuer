####################################################################
# LibOpenSteuer - An open-source taxlibrary based on german tax laws
####################################################################

# This is the main Makefile.  Compile from here.
# Don't use the Makefiles in the subdirectories.

#######################################################################
# Edit these paths, if you want your installation in a different place.
#######################################################################

# PREFIX is the main path to the actual subdirs:
PREFIX = /usr/local

# LIB_DIR is the path to the directory where the library goes:
export LIB_DIR = $(PREFIX)/lib

# INCLUDE_DIR is the path to the directory where the headers go:
export INCLUDE_DIR = $(PREFIX)/include/opensteuer

# MAN3_DIR is the path to the directory, where the man(3)-pages go:
export MAN3_DIR = $(PREFIX)/man/man3

###############################################################
# Programs we need for the installation.  Make sure they exist.
###############################################################

export SHELL = /bin/sh
export INSTALL = /usr/bin/install
export RMDIR = /bin/rmdir

# for shared libs:
export LDCONFIG = /sbin/ldconfig
export LN = /bin/ln
# for static libs:
export RANLIB = /usr/bin/ranlib

#################################
# For internal use. Don't change.
#################################

export ADA_DIR = ./src/ada
DOC_DIR = ./Documentation
TEST_DIR = ./test

ADA_MAKEFILE = Makefile.Ada
DOC_MAKEFILE = Makefile.Documentation
TEST_MAKEFILE = Makefile.Test

export API = 0
export VERSION = 2004.2

.PHONY: default debug devel
default debug devel:
	@cd $(ADA_DIR) && $(MAKE) -f $(ADA_MAKEFILE) $@

.PHONY: check test
check test: default
	@cd $(TEST_DIR) && $(MAKE) -f $(TEST_MAKEFILE) $@

.PHONY: shared static
shared static:
	@cd $(ADA_DIR) && $(MAKE) -f $(ADA_MAKEFILE) $@

.PHONY: install uninstall
install uninstall:
	@cd $(ADA_DIR) && $(MAKE) -f $(ADA_MAKEFILE) $@
	@cd $(DOC_DIR) && $(MAKE) -f $(DOC_MAKEFILE) $@

.PHONY: clean
clean:
	@cd $(ADA_DIR) && $(MAKE) -f $(ADA_MAKEFILE) $@
	@cd $(DOC_DIR) && $(MAKE) -f $(DOC_MAKEFILE) $@
	@cd $(TEST_DIR) && $(MAKE) -f $(TEST_MAKEFILE) $@
	$(RM) *~
