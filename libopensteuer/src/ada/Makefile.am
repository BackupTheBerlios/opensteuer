# The targets in this directory
lib_LIBRARIES = libopensteuer.a
LIBBASENAME = libopensteuer
all: $(lib_LIBRARIES) $(LIBSONAME)

# Name conventions for the shared library
LIBSONAME = $(LIBBASENAME).so
LIBSONAMEVERSION = $(LIBSONAME).@OPENSTEUER_SO_VERSION@
LIBSONAMEFULL = $(LIBSONAMEVERSION).@OPENSTEUER_SO_MINOR@.@OPENSTEUER_SO_REVISION@

# These are the source files that the static library depends on.
libopensteuer_a_SOURCES = \
 libopensteuer.adb \
 libopensteuer-esttab.adb \
 libopensteuer-lsttab.adb \
 libopensteuer-pap.adb 

# These are the header files (in case of ada: spec *and* body files)
# that are required for programs that want to use the
# library, i.e. these get installed. 
iheaderdir = $(includedir)/opensteuer
iheader_HEADERS = \
 $(libopensteuer_a_SOURCES) \
 libopensteuer-esttab.ads \
 libopensteuer-global.ads \
 libopensteuer-lsttab.ads \
 libopensteuer-pap.ads \
 libopensteuer.ads \
 libopensteuer.h

# Additional files to be removed by 'make clean'
CLEANFILES = *.ali b~*.ad? lib*.so*

# ############################################################
# Hopefully the rules below don't have to be touched anymore.

# This is the rule for building the shared library. For simplicity,
# let the static library be built first.
# Statically link the libgnat.a to the shared library
$(LIBSONAME): $(lib_LIBRARIES)
	$(GNATGCC) -shared $(AM_LDFLAGS) $(LDFLAGS) \
	 -o $(LIBSONAMEFULL) \
	 -Wl,-soname -Wl,$(LIBSONAME) \
	 *.o $(LIBGNAT)
	rm -f $(LIBSONAMEVERSION) $(LIBSONAME)
	$(CP_S) $(LIBSONAMEFULL) $(LIBSONAMEVERSION)
	$(CP_S) $(LIBSONAMEFULL) $(LIBSONAME)

# Rules for installing and uninstalling the shared library.
.PHONY: install-LIBBASENAME-so uninstall-LIBBASENAME-so
install-LIBBASENAME-so: $(LIBSONAME)
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(libdir)
	$(libLIBRARIES_INSTALL) $(LIBSONAMEFULL) $(DESTDIR)$(libdir)/$(LIBSONAMEFULL); \
	cd $(DESTDIR)$(libdir); \
	rm -f $(LIBSONAMEVERSION) $(LIBSONAME); \
	$(CP_S) $(LIBSONAMEFULL) $(LIBSONAMEVERSION); \
	$(CP_S) $(LIBSONAMEFULL) $(LIBSONAME) 
	@$(POST_INSTALL)
uninstall-LIBBASENAME-so:
	@$(NORMAL_UNINSTALL)
	rm -f $(DESTDIR)$(libdir)/$(LIBSONAME)
	rm -f $(DESTDIR)$(libdir)/$(LIBSONAMEVERSION)
	rm -f $(DESTDIR)$(libdir)/$(LIBSONAMEFULL)
install: install-am install-LIBBASENAME-so 
uninstall: uninstall-am uninstall-LIBBASENAME-so

# The static library gets built by automake's rules. Alternatively,
# something like this would be used:
#libopensteuer.a: $(libopensteuer_a_SOURCES)
#	LIBBASENAME=$(patsubst %.a,%,$@) && \
#		gnatmake $${LIBBASENAME} -c -fPIC -m

SUFFIXES = .adb .o
# Extra suffix rule so that automake knows how .adb (ada) files are
# being compiled to (libtool) object files via libtool.
.adb.o:
	LIBBASENAME=$(patsubst %.o,%,$@) && \
		gnatmake $${LIBBASENAME} -c -fPIC -m

# Extra rule for building the code with the library elaboration
# procedure (named `<prefix>init') and the run-time finalization
# procedure (named `<prefix>final').
#b~$(LIBBASENAME).adb: $(LIBBASENAME).adb
#	gnatbind -L$(LIBBASENAME) $(LIBBASENAME)


# Old rules for libtool:

#SUFFIXES = .adb .lo
#.adb.o:
#	$(LIBTOOL) --mode=compile $(GNATGCC) $(AM_CFLAGS) $(CFLAGS) \
#	  -c -o $@ $< 

# Command for linking the shared library
#LINK = $(LIBTOOL) --mode=link $(GNATGCC) $(AM_LDFLAGS) $(LDFLAGS) -o $@

# And these are the source files for the shared library.
#libopensteuer_la_SOURCES = \
# $(libopensteuer_a_SOURCES) \
# b~libopensteuer.adb

