# The targets in this directory
lib_LTLIBRARIES = libopensteuer.la

# These are the source files that the static library depends on.
libopensteuer_a_SOURCES = \
 libopensteuer-esttab.adb \
 libopensteuer-lsttab.adb \
 libopensteuer-pap.adb \
 libopensteuer.adb 

# And these are the source files for the shared library.
libopensteuer_la_SOURCES = \
 $(libopensteuer_a_SOURCES) 
nodist_libopensteuer_la_SOURCES = \
 b~libopensteuer.adb

# These are the header files (in case of ada: spec *and* body files)
# that are required for programs that want to use the
# library, i.e. these get installed. The prefix 'pkginclude' means
# these will be installed in $prefix/include/libopensteuer.
pkginclude_HEADERS = \
 $(libopensteuer_a_SOURCES) \
 libopensteuer-esttab.ads \
 libopensteuer-global.ads \
 libopensteuer-lsttab.ads \
 libopensteuer-pap.ads \
 libopensteuer.ads \
 libopensteuer.h

# Additional flags for linking the shared library can go here
libopensteuer_la_LIBADD =
#libopensteuer_la_LIBADD = -lgnat <-- will be required by each executable :-(
libopensteuer_la_LDFLAGS= -version-info \
  @OPENSTEUER_SO_VERSION@:@OPENSTEUER_SO_REVISION@:@OPENSTEUER_SO_AGE@

# Additional files to be removed by 'make clean'
CLEANFILES = *.ali b~*.ad?

# ############################################################
# Hopefully the rules below don't have to be touched anymore.

# Command for linking the shared library
LINK = $(LIBTOOL) --mode=link $(GNATGCC) $(AM_LDFLAGS) $(LDFLAGS) -o $@

# Extra suffix rule so that automake knows how .adb (ada) files are
# being compiled to (libtool) object files via libtool.
.adb.lo:
	$(LIBTOOL) --mode=compile $(GNATGCC) $(AM_CFLAGS) $(CFLAGS) \
	  -c -o $@ $< 

# Extra rule for building the code with the library elaboration
# procedure (named `<prefix>init') and the run-time finalization
# procedure (named `<prefix>final').
b~%.adb : %.adb ; 
	LIBBASENAME=$(patsubst %.adb,%,$<) && \
		gnatbind -L$${LIBBASENAME} $${LIBBASENAME}
