# ---------------------------------------------------------------
# OpenSteuer - An open-source taxprogram based on german tax laws
# ---------------------------------------------------------------

SHELL = /bin/sh
INSTALL = /usr/bin/install

MAIN = opensteuer
MAN_PAGE = $(MAIN).1

# Edit these paths, if you want your installation in a different place:

# PREFIX is the main path to the actual subdirs for our program:
PREFIX = /usr/local

# INSTALL_DIR is the path to the directory where the binary goes:
INSTALL_DIR = $(PREFIX)/bin

# MAN_DIR is the path to the man-directory:
MAN_DIR = $(PREFIX)/man

# MAN1_DIR is the path to the directory, where the man(1)-pages go:
MAN1_DIR = $(MAN_DIR)/man1
	
all :
	gnatmake $(MAIN) -m `gtkada-config` -cargs -O3

static :
	gnatmake $(MAIN) -m `gtkada-config --static` -cargs -O3 -bargs -static

debug :
	gnatmake $(MAIN) -gnatafo -g -gstabs -m `gtkada-config`

devel :
	gnatmake $(MAIN) -gnatafo -m `gtkada-config`

devel-force :
	gnatmake $(MAIN) -gnatafo -f -m `gtkada-config`
	
dirs :
	@if ! [ -d $(INSTALL_DIR) ]; then \
	      	echo make dirs: Creating $(INSTALL_DIR); \
		$(INSTALL) -d -m 755 $(INSTALL_DIR); \
	fi
	@if ! [ -d $(MAN1_DIR) ]; then \
	      	echo make dirs: Creating $(MAN1_DIR); \
		$(INSTALL) -d -m 755 $(MAN1_DIR); \
	fi
	
install : all
	@echo make: Installing $(MAIN) in $(INSTALL_DIR)/$(MAIN)
	@if ! $(INSTALL) $(MAIN) $(INSTALL_DIR)/; then \
	    if ! [ -d $(INSTALL_DIR) ]; then \
		echo make install: The directory $(INSTALL_DIR) does not exist.; \
		echo make install: $(MAIN) could not be installed.; \
		echo make install: To create $(INSTALL_DIR), run make dirs install.; \
	    fi; \
	fi
	@echo make: Installing $(MAN_PAGE) in $(MAN1_DIR)/$(MAN_PAGE)
	@if ! $(INSTALL) -m 644 $(MAN_PAGE) $(MAN1_DIR)/; then \
	    if ! [ -d $(INSTALL_DIR) ]; then \
		echo make install: The directory $(MAN1_DIR) does not exist.; \
		echo make install: The man-page could not be installed.; \
		echo make install: To create $(MAN1_DIR), run make dirs install.; \
	    fi; \
	fi

uninstall :
	$(RM) $(INSTALL_DIR)/$(MAIN) $(MAN1_DIR)/$(MAN_PAGE)

clean : 
	$(RM) $(MAIN) *.o *.ali debug.txt *~

.PHONY : all clean debug devel devel-force dirs install static uninstall
