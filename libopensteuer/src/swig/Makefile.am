EXTRA_DIST = libopensteuer.i 

LIBOPENSTEUER_DIR = ../ada
OST_INCLUDES = $(LIBOPENSTEUER_DIR)
LIBOPENSTEUER_LIBDIR = $(LIBOPENSTEUER_DIR)
LIBBASENAME = libopensteuer

all: _$(LIBBASENAME).so
test: test_python

_$(LIBBASENAME).so: $(LIBBASENAME)_wrap.o
	$(CC) -shared $(AM_LDFLAGS) $(LDFLAGS) \
	 -L$(LIBOPENSTEUER_LIBDIR) -lopensteuer \
	 $< \
	 -o $@

$(LIBBASENAME)_wrap.o: $(LIBBASENAME).py
	$(CC) -c -fPIC $(AM_CFLAGS) $(CFLAGS) \
	 -I$(OST_INCLUDES) \
	 $(PYTHON_INCLUDES) \
	 $(LIBBASENAME)_wrap.c 

test_python: all
	LD_LIBRARY_PATH=.:$(LIBOPENSTEUER_LIBDIR) python test.py

.i.py:
	swig -python $<

install:
	@echo "Sorry, no python installation prepared yet, only test"

SUFFIXES = .py .i _wrap.o .so

CLEANFILES = *_wrap.* $(LIBBASENAME).py *.pyc *.so
