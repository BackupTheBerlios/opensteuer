# ---------------------------------------------------------------
# OpenSteuer - An open-source taxprogram based on german tax laws
# ---------------------------------------------------------------

SHELL = /bin/sh
MAIN = opensteuer

.PHONY: all
all:
	gnatmake $(MAIN) -m `gtkada-config` -cargs -O3

.PHONY: static
static:
	gnatmake $(MAIN) -m `gtkada-config --static` -cargs -O3 -bargs -static

.PHONY: debug
debug:
	gnatmake $(MAIN) -gnatafo -g -gstabs -m `gtkada-config`

.PHONY: devel
devel:
	gnatmake $(MAIN) -gnatafo -m `gtkada-config`

.PHONY: devel-force
devel-force:
	gnatmake $(MAIN) -gnatafo -f -m `gtkada-config`

.PHONY: clean
clean: 
	$(RM) $(MAIN) *.o *.ali debug.txt *~
