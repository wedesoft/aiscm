AC_PREREQ([2.69])
AC_INIT([aiscm], [0.2.3], [jan@wedesoft.de])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([1.14])
AC_CONFIG_MACRO_DIR([m4])
AC_REQUIRE_AUX_FILE([tap-driver.sh])
AM_EXTRA_RECURSIVE_TARGETS([bench])
LT_INIT
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_AWK
PKG_PROG_PKG_CONFIG
AM_CONFIG_HEADER(config.h)
AC_CHECK_PROG(TOUCH,touch,touch,touch)
AC_CHECK_PROG(PANDOC,pandoc,pandoc,no)
if test "x$PANDOC" = xno; then
  AC_MSG_ERROR([Could not find 'pandoc' in path.])
fi
AC_MSG_CHECKING([wether C compiler works])
AC_TRY_LINK([],[],[AC_MSG_RESULT([yes])],[AC_MSG_ERROR([Failed to compile and link C program.])])
AC_CHECK_HEADERS([assert.h stdio.h stdlib.h string.h])
AC_FUNC_MALLOC
AC_CHECK_FUNCS([strcmp])

PKG_CHECK_MODULES(MJPEG, mjpegtools >= 1.9.0)
AC_SUBST(MJPEG_CFLAGS)
AC_SUBST(MJPEG_LIBS)

PKG_CHECK_MODULES(SWSCALE, libswscale >= 2.1.0)
AC_SUBST(SWSCALE_CFLAGS)
AC_SUBST(SWSCALE_LIBS)

PKG_CHECK_MODULES(GUILE, guile-2.0 >= 2.0.9)
GUILE_PROGS
GUILE_SITE_DIR
GUILE_EXT_DIR
GUILE_CACHE_DIR

AC_PATH_XTRA
if test "x$no_x" = xyes; then
  AC_MSG_ERROR([Could not find X11 libraries.])
fi
AC_SUBST(X_CFLAGS)
AC_SUBST(X_LIBS)

AX_HAVE_OPENGL
AC_MSG_CHECKING([for GL])
if test "x$have_GL" != "xyes"; then
  AC_MSG_RESULT([no])
  AC_MSG_ERROR([Could not find GL])
else
  AC_MSG_RESULT([yes])
fi
AC_MSG_CHECKING([for GLU])
if test "x$have_GLU" != "xyes"; then
  AC_MSG_RESULT([no])
  AC_MSG_ERROR([Could not find GLU])
else
  AC_MSG_RESULT([yes])
fi

AC_MSG_CHECKING([for Xvideo])
LIBS="$LIBS -lXv"
AC_TRY_LINK([#include <X11/extensions/Xv.h>],
   [XvQueryPortAttributes(0, 0, 0)],
   [AC_MSG_RESULT([yes])],
   [AC_MSG_RESULT([no])]
   [AC_MSG_ERROR([Could not locate Xvideo extension])])

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
          bench/Makefile
          doc/Makefile
          doc/index.md
          doc/installation.md,
          echo "$info",
          info="$info")
