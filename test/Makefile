SHELL = /bin/sh
MAIN = test_libopensteuer

#LIBOPENSTEUER_DIR = /usr/local/include/libopensteuer
LIBOPENSTEUER_DIR = $(HOME)/usr
OST_INCLUDES = $(LIBOPENSTEUER_DIR)/include/libopensteuer
LIBOPENSTEUER_LIBDIR = $(LIBOPENSTEUER_DIR)/lib

.PHONY : all
all : test testc

.PHONY : test
test : $(MAIN)
	LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):$(LIBOPENSTEUER_LIBDIR) ./$(MAIN)

$(MAIN) :
	gnatmake -I$(OST_INCLUDES) $(MAIN) -m -cargs -O3 -largs -L$(LIBOPENSTEUER_LIBDIR) -lopensteuer

.PHONY : clean
clean : 
	$(RM) $(MAIN) *.o *.ali *~ testc_libopensteuer

.PHONY : testc
testc: testc_libopensteuer
	LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):$(LIBOPENSTEUER_LIBDIR) ./testc_libopensteuer 10000

testc_libopensteuer: testc_libopensteuer.c $(OST_INCLUDES)/libopensteuer.h
	gcc -g -c -Wall -pedantic -Werror -I$(OST_INCLUDES) testc_libopensteuer.c
	gcc -g -o testc_libopensteuer testc_libopensteuer.o -L$(LIBOPENSTEUER_LIBDIR) -lopensteuer #-lgnat
