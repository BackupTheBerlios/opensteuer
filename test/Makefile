SHELL = /bin/sh
MAIN = test_libopensteuer

LIBOPENSTEUER_DIR = /usr/local/include/libopensteuer

.PHONY : all
all :
	gnatmake -I$(LIBOPENSTEUER_DIR) $(MAIN) -m -cargs -O3 -largs -lopensteuer

.PHONY : clean
clean : 
	$(RM) $(MAIN) *.o *.ali *~
