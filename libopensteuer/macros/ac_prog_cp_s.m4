dnl Available from the GNU Autoconf Macro Archive at:
dnl http://www.gnu.org/software/ac-archive/htmldoc/ac_prog_cp_s.html
dnl
AC_DEFUN([AC_PROG_CP_S],
[AC_REQUIRE([AC_PROG_LN_S])dnl
AC_MSG_CHECKING(whether cp -s works)
AC_CACHE_VAL(ac_cv_prog_CP_S,
[rm -f conftestdata
if cp -s X conftestdata 2>/dev/null
then
  rm -f conftestdata
  ac_cv_prog_CP_S="cp -s"
else
  ac_cv_prog_CP_S=cp
fi
if test "$LN_S" = "ln -s" ; then
  ac_cv_prog_CP_S="ln -s"
fi])dnl
CP_S="$ac_cv_prog_CP_S"
if test "$ac_cv_prog_CP_S" = "ln -s"; then
  AC_MSG_RESULT(using ln -s)
elif test "$ac_cv_prog_CP_S" = "cp -s"; then
  AC_MSG_RESULT(yes)
else
  AC_MSG_RESULT(no, using cp)
fi
AC_SUBST(CP_S)dnl
])
