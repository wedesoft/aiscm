AC_PREREQ([2.69])
AC_INIT([aiscm], [0.1.4], [jan@wedesoft.de])
AM_INIT_AUTOMAKE([1.14])
AC_CONFIG_MACRO_DIR([m4])
LT_INIT
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_AWK
PKG_PROG_PKG_CONFIG
AM_CONFIG_HEADER(config.h)
AC_CHECK_PROG(TOUCH,touch,touch)
AC_CHECK_PROG(PANDOC,pandoc,pandoc)
AC_CHECK_HEADERS([assert.h stdio.h stdlib.h string.h])
AC_FUNC_MALLOC
AC_CHECK_FUNCS([strcmp])
PKG_CHECK_MODULES([GUILE], [guile-2.0])
GUILE_PROGS
GUILE_SITE_DIR
GUILE_EXT_DIR
GUILE_CACHE_DIR

dnl Switch for debug or release mode.
AC_ARG_ENABLE(debug,
  [  --enable-debug=DEBUG    compile with debugging assertions (default=no)],
  enable_debug=$enableval, enable_debug=no)
if test "x$enable_debug" == "xyes"; then
  AM_CFLAGS='-Wall'
else
  AM_CFLAGS='-Wall -DNDEBUG'
fi
AC_SUBST(AM_CFLAGS)

info="$info

prefix                               : $prefix
debug mode                           : $enable_debug"

AC_OUTPUT(Makefile
          aiscm/Makefile
          tests/Makefile
          doc/Makefile,
          echo "$info",
          info="$info")

