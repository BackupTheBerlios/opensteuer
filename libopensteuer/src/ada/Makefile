# ------------------------------------------------------------------
# LibOpenSteuer - An open-source taxlibrary based on german tax laws
# ------------------------------------------------------------------

# This is the Makefile for the Ada-source.

# ------------------------------------------------------------------
# Don't make any changes here until you know what you're doing.
# Changes should only be made to the Makefile in the main dir.
# ------------------------------------------------------------------
# Don't compile directly from here. Paths will probably be wrong.
# Use the Makefile in the main directory for compiling/installation.
# ------------------------------------------------------------------

MAIN = libopensteuer
ADBS = *.ad[bs]

SHARED = $(MAIN).so
SHARED_API = $(SHARED).$(API)
SHARED_VERSION = $(SHARED_API).$(VERSION)
STATIC = $(MAIN).a
ALL_LIBS = $(SHARED) $(SHARED_API) $(SHARED_VERSION) $(STATIC)

# ---------------------
# -- nur kompilieren --
# ---------------------

.PHONY : all
all :
	gnatmake $(MAIN) -c -fPIC -m -O3

.PHONY : debug
debug :
	gnatmake $(MAIN) -c -fPIC -gnatafo -g -gstabs -m

.PHONY : devel
devel :
	gnatmake $(MAIN) -c -fPIC -gnatafo -m

# ----------------
# -- shared lib --
# ----------------

.PHONY: shared
shared: all
	gnatgcc -shared -Wl,-soname,$(SHARED_API) -o $(SHARED_VERSION) $(MAIN).o

.PHONY: shared_install
shared_install: shared
	@echo make: Installing $(SHARED_VERSION) in $(LIB_DIR)
	@if ! [ -d $(LIB_DIR) ]; then \
		echo make: Creating $(LIB_DIR); \
		$(INSTALL) -d -m 755 $(LIB_DIR); \
	fi
	$(INSTALL) -m 644 $(SHARED_VERSION) $(LIB_DIR)/
	$(LDCONFIG) # -n $(LIB_DIR)

# -------------------
# -- statische lib --
# -------------------

.PHONY: static
static: all
	ar rc $(STATIC) $(MAIN).o

.PHONY: static_install
static_install: static
	@echo make: Installing $(STATIC) in $(LIB_DIR)
	@if ! [ -d $(LIB_DIR) ]; then \
		echo make: Creating $(LIB_DIR); \
		$(INSTALL) -d -m 755 $(LIB_DIR); \
	fi
	$(INSTALL) -m 644 $(STATIC) $(LIB_DIR)/
	$(RANLIB) $(LIB_DIR)/$(STATIC)

# ----------------------------------------------------------------------
# -- die libopensteuer-dev-Version, also shared-lib mit Quellcode in  --
# -- $(PREFIX)/include und libopensteuer.so als link auf *.so.1.2.3.4 --
# ----------------------------------------------------------------------

.PHONY: shared_devel_install
shared_devel_install: shared_install
	@echo make: Installing $(ADBS) in $(INCLUDE_DIR)
	@if ! [ -d $(INCLUDE_DIR) ]; then \
		echo make: Creating $(INCLUDE_DIR); \
		$(INSTALL) -d -m 755 $(INCLUDE_DIR); \
	fi
	$(INSTALL) -m 644 *.ad[bs] $(INCLUDE_DIR)
	cd $(LIB_DIR) && $(LN) -s $(SHARED_VERSION) $(SHARED)

# ---------------
# -- aufräumen --
# ---------------

.PHONY: uninstall
uninstall:
	$(if $(LIB_DIR), cd $(LIB_DIR) && $(RM) $(ALL_LIBS))
	@if [ -d $(INCLUDE_DIR) ]; then \
		$(if $(INCLUDE_DIR), cd $(INCLUDE_DIR) && $(RM) $(ADBS)); \
		$(if $(INCLUDE_DIR), $(RMDIR) $(INCLUDE_DIR)); \
	fi

.PHONY : clean
clean : 
	$(RM) $(MAIN) $(ALL_LIBS) *.ali *.o *~
