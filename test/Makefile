SHELL = /bin/sh
MAIN = test_libopensteuer

#LIBOPENSTEUER_DIR = /usr/local/include/libopensteuer
LIBOPENSTEUER_DIR = ../libopensteuer/src/ada

.PHONY : all
all :
	gnatmake -I$(LIBOPENSTEUER_DIR) $(MAIN) -m -cargs -O3 -largs -lopensteuer

.PHONY : clean
clean : 
	$(RM) $(MAIN) *.o *.ali *~ testc_libopensteuer

testc: testc_libopensteuer
	LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):$(LIBOPENSTEUER_DIR) ./testc_libopensteuer 10000

testc_libopensteuer: testc_libopensteuer.c $(LIBOPENSTEUER_DIR)/libopensteuer.h
	gcc -g -c -Wall -pedantic -Werror -I$(LIBOPENSTEUER_DIR) testc_libopensteuer.c
	gcc -g -o testc_libopensteuer testc_libopensteuer.o -lopensteuer -L$(LIBOPENSTEUER_DIR) -lgnat -L/usr/lib/gcc-lib/i586-suse-linux/3.3.1/adalib
