# ---------------------------------------------------------------
# OpenSteuer - An open-source taxprogram based on german tax laws
# ---------------------------------------------------------------

# ---------------------------------------------------------------------
# Edit these paths, if you want your installation in a different place:
# ---------------------------------------------------------------------

# PREFIX is the main path to the actual subdirs for our program:
PREFIX = /usr/local

# INSTALL_DIR is the path to the directory where the binary goes:
INSTALL_DIR = $(PREFIX)/bin

# MAN_DIR is the path to the man-directory:
MAN_DIR = $(PREFIX)/man

# MAN1_DIR is the path to the directory, where the man(1)-pages go:
MAN1_DIR = $(MAN_DIR)/man1

SHELL = /bin/sh
INSTALL = /usr/bin/install

# -------------------------------
# For internal use. Don't change:
# -------------------------------

MAIN = opensteuer
MAIN_SOURCE_DIR = ./src/ada

MAN_PAGE = $(MAIN).1
MAN_PAGE_DIR = ./Documentation

.PHONY: all
all:
	cd $(MAIN_SOURCE_DIR) && $(MAKE) all

.PHONY: static
static:
	cd $(MAIN_SOURCE_DIR) && $(MAKE) static

.PHONY: debug
debug:
	cd $(MAIN_SOURCE_DIR) && $(MAKE) debug

.PHONY: devel
devel:
	cd $(MAIN_SOURCE_DIR) && $(MAKE) devel

.PHONY: devel-force
devel-force:
	cd $(MAIN_SOURCE_DIR) && $(MAKE) devel-force

.PHONY: dirs
dirs:
	@if ! [ -d $(INSTALL_DIR) ]; then \
	      	echo make dirs: Creating $(INSTALL_DIR); \
		$(INSTALL) -d -m 755 $(INSTALL_DIR); \
	fi
	@if ! [ -d $(MAN1_DIR) ]; then \
	      	echo make dirs: Creating $(MAN1_DIR); \
		$(INSTALL) -d -m 755 $(MAN1_DIR); \
	fi

.PHONY: install
install: all
	@echo make: Installing $(MAIN) in $(INSTALL_DIR)/$(MAIN)
	@if ! $(INSTALL) $(MAIN_SOURCE_DIR)/$(MAIN) $(INSTALL_DIR)/; then \
	    if ! [ -d $(INSTALL_DIR) ]; then \
		echo make install: The directory $(INSTALL_DIR) does not exist.; \
		echo make install: $(MAIN) could not be installed.; \
		echo make install: To create $(INSTALL_DIR), run make dirs install.; \
	    fi; \
	fi
	@echo make: Installing $(MAN_PAGE) in $(MAN1_DIR)/$(MAN_PAGE)
	@if ! $(INSTALL) -m 644 $(MAN_PAGE_DIR)/$(MAN_PAGE) $(MAN1_DIR)/; then \
	    if ! [ -d $(INSTALL_DIR) ]; then \
		echo make install: The directory $(MAN1_DIR) does not exist.; \
		echo make install: The man-page could not be installed.; \
		echo make install: To create $(MAN1_DIR), run make dirs install.; \
	    fi; \
	fi

.PHONY: uninstall
uninstall:
	$(RM) $(INSTALL_DIR)/$(MAIN) $(MAN1_DIR)/$(MAN_PAGE)

.PHONY: clean
clean: 
	cd $(MAIN_SOURCE_DIR) && $(MAKE) clean
	$(RM) *~
