SHELL = /bin/sh

INCLUDE_DIR = /usr/local/include/opensteuer
LIB_DIR = /usr/local/lib

.PHONY: run
run: ada c
	./test_ada
	./test_c

.PHONY: ada
ada:
	gnatmake -I$(INCLUDE_DIR) test_ada -largs -lopensteuer

.PHONY: c
c:
	gcc -Wall -o test_c test_c.c -I$(INCLUDE_DIR) -lopensteuer -lgnat

.PHONY: clean
clean: 
	$(RM) *.o *.ali *~ test_ada test_c
