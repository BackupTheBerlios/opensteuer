dnl Process this file with autoconf to produce a configure script.

dnl Start autoconf processing. 
dnl The argument is simply some unique file in the source dir.
AC_INIT(README)

dnl Checking host/target/build systems, for make, install etc.
AC_CANONICAL_SYSTEM
dnl Perform program name transformation
AC_ARG_PROGRAM

dnl Automake doc recommends to do this only here. This sets the name
dnl and version number of the package.
AM_INIT_AUTOMAKE(libopensteuer,0.1)
AC_CONFIG_HEADERS(config.h)

# Version number for the library. Note that the VERSION above in
# AM_INIT_AUTOMAKE should manually be set to some similar value. Also
# note that the three numbers below are the libtool versioning numbers
# which are *not* directly translated to .so.x.y.z numbers.
OPENSTEUER_SO_VERSION=0
OPENSTEUER_SO_MINOR=0
OPENSTEUER_SO_REVISION=1
AC_SUBST(OPENSTEUER_SO_VERSION)
AC_SUBST(OPENSTEUER_SO_MINOR)
AC_SUBST(OPENSTEUER_SO_REVISION)

dnl Set the program name for make
AC_PROG_MAKE_SET

dnl Make /usr/local the default for the installation
AC_PREFIX_DEFAULT(/usr/local)
AM_MAINTAINER_MODE

### -------------------------------------------------------------

dnl Checks for standard programs.
AC_PROG_RANLIB
AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_CP_S

dnl Init libtool
#AC_PROG_LIBTOOL
#
#dnl Additionally, libtool needs some fixing because one of our 
#dnl object file names includes a tilde
#patch < libtool-tilde.patch -- this patch is only for specific libtool versions
#AC_MSG_NOTICE([Correcting libtool for tilde in filenames...])
#sed -e ['s/\([^\]\)~/\1!/g'] libtool > libtool.new
#mv libtool libtool.orig
#mv libtool.new libtool
#AC_MSG_NOTICE([done])
#
#dnl Create both shared and static libraries by libtool
#AC_ENABLE_STATIC(yes)
#AC_ENABLE_SHARED(yes)

### -------------------------------------------------------------
### Specific packages
### -------------------------------------------------------------

dnl Check for GNATGCC. If not found, set it to gcc (which is in $CC)
AC_CHECK_PROG(GNATGCC, gnatgcc, gnatgcc, "$CC")

dnl Add extra configure switches for enabling of compile warnings.
ACX_COMPILE_WARN

dnl Look for libgnat.a
AC_MSG_CHECKING(for static library libgnat.a)
GNATLS=gnatls
LIBGNAT=""
AC_ARG_WITH(libgnat,
  [  --with-libgnat=LIBRARY fully qualified path to libgnat.a ],
  [LIBGNAT="$withval"],
  [ for A in `$GNATLS -v`; do
	if test -d $A; then
	    if test -r $A/libgnat.a; then
		LIBGNAT=$A/libgnat.a
	    fi
	fi
    done 
])
LIBGNAT=`echo $LIBGNAT|sed -e 's!//!/!g'`
AC_MSG_RESULT($LIBGNAT)
AC_SUBST(LIBGNAT)

dnl Check for python
AC_CHECK_PROG(PYTHON, python, python, "")
AC_MSG_CHECKING([for python include arguments])
PYTHON_INCLUDES=""
PYTHON_LIBDIR=""
SWIG_SRCDIR=""
if test -n $PYTHON; then
   pythoninc_dir=`echo -e "from distutils.sysconfig import get_python_inc\nprint get_python_inc()\n" | $PYTHON `
   PYTHON_INCLUDES="-I$pythoninc_dir"
   PYTHON_LIBDIR=`echo -e "from distutils.sysconfig import get_python_lib\nprint get_python_lib()\n" | $PYTHON `
   SWIG_SRCDIR="swig"
fi
AC_MSG_RESULT($PYTHON_INCLUDES)
AC_SUBST(PYTHON_INCLUDES)
AC_SUBST(PYTHON_LIBDIR)
AC_SUBST(SWIG_SRCDIR)

### -------------------------------------------------------------
### Output files
### -------------------------------------------------------------

AC_OUTPUT( \
 Makefile \
 macros/Makefile \
 src/Makefile \
 src/ada/Makefile \
 src/swig/Makefile \
)
