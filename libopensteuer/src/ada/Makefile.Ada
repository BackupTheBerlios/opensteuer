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

# Filenames:
MAIN = libopensteuer
SOURCE = *.ad[bs] *.h

SHARED = $(MAIN).so
SHARED_API = $(SHARED).$(API)
SHARED_VERSION = $(SHARED_API).$(VERSION)
STATIC = $(MAIN).a
ALL_LIBS = $(SHARED) $(SHARED_API) $(SHARED_VERSION) $(STATIC)

# Error-Text:
ERR = not set.  Run make from the main directory

# Just compile:

.PHONY: all
all:
	gnatmake $(MAIN) -c -fPIC -m -O3

.PHONY: debug
debug:
	gnatmake $(MAIN) -c -fPIC -gnatafo -g -gstabs -m

.PHONY: devel
devel:
	gnatmake $(MAIN) -c -fPIC -gnatafo -m

# Build the library:

.PHONY: shared
shared: all
ifndef API
	$(error 'API' $(ERR))
endif
ifndef VERSION
	$(error 'VERSION' $(ERR))
endif
	$(CC) -shared -Wl,-soname,$(SHARED_API) -o $(SHARED_VERSION) *.o

.PHONY: static
static: all
	ar rc $(STATIC) $(MAIN).o

# Install:

.PHONY: install
install:
ifndef LIB_DIR
	$(error 'LIB_DIR' $(ERR))
endif
ifndef INCLUDE_DIR
	$(error 'INCLUDE_DIR' $(ERR))
endif
	@if ! [ -d $(LIB_DIR) ]; then \
		$(INSTALL) -d -m 755 $(LIB_DIR); \
	fi
	@if ! [ -d $(INCLUDE_DIR) ]; then \
		$(INSTALL) -d -m 755 $(INCLUDE_DIR); \
	fi
	@if [ -f $(SHARED_VERSION) ]; then \
		$(INSTALL) -m 644 $(SHARED_VERSION) $(LIB_DIR)/; \
		cd $(LIB_DIR) && $(RM) $(SHARED); \
		cd $(LIB_DIR) && $(LN) -s $(SHARED_VERSION) $(SHARED); \
		cd $(LIB_DIR) && $(RM) $(SHARED_API); \
		cd $(LIB_DIR) && $(LN) -s $(SHARED_VERSION) $(SHARED_API); \
		$(LDCONFIG); \
	elif [ -f $(STATIC) ]; then \
		$(INSTALL) -m 644 $(STATIC) $(LIB_DIR)/ \
		$(RANLIB) $(LIB_DIR)/$(STATIC); \
	else \
		echo "make install: WARNING: No libraries found.  Only source will be installed."; \
	fi
	$(INSTALL) -m 644 $(SOURCE) $(INCLUDE_DIR)

# Clean-up:

.PHONY: uninstall
uninstall:
	$(if $(LIB_DIR), cd $(LIB_DIR) && $(RM) $(ALL_LIBS))
	if [ -d $(INCLUDE_DIR) ]; then \
		$(if $(INCLUDE_DIR), cd $(INCLUDE_DIR) && $(RM) $(SOURCE)); \
		$(if $(INCLUDE_DIR), $(RMDIR) $(INCLUDE_DIR)); \
	fi

.PHONY: clean
clean: 
	$(RM) $(MAIN) $(SHARED)* $(STATIC) *.ali *.o *~
