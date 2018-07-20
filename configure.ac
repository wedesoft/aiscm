dnl Run `./bootstrap` to generate a configure script.
dnl Then run `./configure` to generate a Makefile.
dnl Finally run `make` to build the project.
AC_PREREQ([2.69])
AC_INIT([aiscm], [0.15.5], [jan@wedesoft.de])
DATE=2018-06-03
AC_SUBST(DATE)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([--warnings=no-portability foreign])
AC_CONFIG_MACRO_DIR([m4])
AC_REQUIRE_AUX_FILE([test-driver.scm])
AM_EXTRA_RECURSIVE_TARGETS([bench recheck integration reintegration])
LT_INIT
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_AWK
AC_LANG(C)
PKG_PROG_PKG_CONFIG
AM_CONFIG_HEADER(config.h)
AC_CHECK_PROG(TOUCH,touch,touch,touch)
AC_CHECK_PROG(PANDOC,pandoc,pandoc,no)
if test "x$PANDOC" = xno; then
  AC_MSG_ERROR([Could not find 'pandoc' in path.])
fi
AC_CHECK_PROG(CONVERT,convert,convert,no)
if test "x$CONVERT" = xno; then
  AC_MSG_ERROR([Could not find 'convert' in path.])
fi
AC_MSG_CHECKING([wether C compiler works])
AC_TRY_LINK([],[],[AC_MSG_RESULT([yes])],[AC_MSG_ERROR([Failed to compile and link C program.])])
AC_CHECK_HEADERS([assert.h stdio.h stdlib.h string.h])
AC_FUNC_MALLOC
AC_CHECK_FUNCS([strcmp])

AX_LLVMC([orcjit])

AX_PTHREAD([],[AC_MSG_ERROR([Could not find pthread library])])
AC_SUBST(PTHREAD_CFLAGS)
AC_SUBST(PTHREAD_LIBS)

PKG_CHECK_MODULES(SWSCALE, libswscale >= 2.1.0)
AC_SUBST(SWSCALE_CFLAGS)
AC_SUBST(SWSCALE_LIBS)

PKG_CHECK_MODULES(SWRESAMPLE, libswresample >= 1.1.100,
  [AC_DEFINE([HAVE_SWRESAMPLE], 1, [libswresample is present])],
  [PKG_CHECK_MODULES(AVRESAMPLE, libavresample >= 1.0.1)])
AC_SUBST(SWRESAMPLE_CFLAGS)
AC_SUBST(SWRESAMPLE_LIBS)
AC_SUBST(AVRESAMPLE_CFLAGS)
AC_SUBST(AVRESAMPLE_LIBS)

PKG_CHECK_MODULES(MJPEG, mjpegtools >= 1.9.0)
AC_SUBST(MJPEG_CFLAGS)
AC_SUBST(MJPEG_LIBS)

PKG_CHECK_MODULES(MAGICK, ImageMagick >= 6.7.7)
AC_SUBST(MAGICK_CFLAGS)
AC_SUBST(MAGICK_LIBS)

PKG_CHECK_MODULES(PULSE, libpulse-simple >= 1.1)
AC_SUBST(PULSE_CFLAGS)
AC_SUBST(PULSE_LIBS)

PKG_CHECK_MODULES(AVUTIL, libavutil >= 52.3.0)
AC_SUBST(AVUTIL_CFLAGS)
AC_SUBST(AVUTIL_LIBS)

PKG_CHECK_MODULES(AVCODEC, libavcodec >= 54.35.0)
AC_SUBST(AVCODEC_CFLAGS)
AC_SUBST(AVCODEC_LIBS)

PKG_CHECK_MODULES(AVFORMAT, libavformat >= 54.20.4)
AC_SUBST(AVFORMAT_CFLAGS)
AC_SUBST(AVFORMAT_LIBS)

old_LIBS="$LIBS"
LIBS="$LIBS $AVFORMAT_LIBS $AVCODEC_LIBS $AVUTIL_LIBS"
AC_MSG_CHECKING([for av_frame_alloc])
AC_TRY_LINK([#include <libavutil/frame.h>],
            [av_frame_alloc()],
            [AC_MSG_RESULT([yes])
             AC_DEFINE(HAVE_FRAME_ALLOC,1,[av_frame_alloc is supported])],
            [AC_MSG_RESULT([no])
             AC_MSG_CHECKING([for avcodec_alloc_frame])
             AC_TRY_LINK([#include <libavcodec/avcodec.h>],
                         [avcodec_alloc_frame()],
                         [AC_MSG_RESULT([yes])],
                         [AC_MSG_RESULT([no])
                          AC_MSG_ERROR([No support for av_frame_alloc or avcodec_alloc_frame])])])
AC_MSG_CHECKING([for av_image_get_buffer_size])
AC_TRY_LINK([#include <libavutil/imgutils.h>],
            [av_image_get_buffer_size(0, 320, 240, 32)],
            [AC_MSG_RESULT([yes])
             AC_DEFINE(HAVE_IMAGE_BUFFER_SIZE,1,[use av_image_get_buffer_size to get buffer size])],
            [AC_MSG_RESULT([no])
             AC_MSG_CHECKING([for avpicture_get_size])
             AC_TRY_LINK([#include <libavutil/imgutils.h>],
                         [avpicture_get_size(0, 320, 240)],
                         [AC_MSG_RESULT([yes])],
                         [AC_MSG_RESULT([no])
                          AC_MSG_ERROR([No support for av_image_get_buffer_size or avpicture_get_size])])])
AC_MSG_CHECKING([for avformat_alloc_output_context2])
AC_TRY_LINK([#include <libavformat/avformat.h>],
            [avformat_alloc_output_context2(0, 0, 0, 0)],
            [AC_MSG_RESULT([yes])
             AC_DEFINE(HAVE_AVFORMAT_ALLOC_OUTPUT_CONTEXT2,1,[use avformat_alloc_output_context2 to initialise output context])],
            [AC_MSG_RESULT([no])])
AC_MSG_CHECKING([for av_frame_get_buffer])
AC_TRY_LINK([#include <libavutil/frame.h>],
            [av_frame_get_buffer(0, 32)],
            [AC_MSG_RESULT([yes])
             AC_DEFINE(HAVE_AV_FRAME_GET_BUFFER,1,[use av_frame_get_buffer to initialise output frames])],
            [AC_MSG_RESULT([no])])
AC_MSG_CHECKING([for av_frame_make_writable])
AC_TRY_LINK([#include <libavutil/frame.h>],
            [av_frame_make_writable(0)],
            [AC_MSG_RESULT([yes])
             AC_DEFINE(HAVE_AV_FRAME_MAKE_WRITABLE,1,[call to av_frame_make_writable required])],
            [AC_MSG_RESULT([no])])
AC_MSG_CHECKING([AVFrame has pts])
AC_TRY_LINK([#include <libavutil/frame.h>],
            [AVFrame *frame; frame->pts],
            [AC_MSG_RESULT([yes])
             AC_DEFINE(HAVE_AV_FRAME_PTS,1,[AVFrame has pts])],
            [AC_MSG_RESULT([no])])
AC_MSG_CHECKING([for av_packet_rescale_ts])
AC_TRY_LINK([#include <libavcodec/avcodec.h>],
            [AVRational rational; av_packet_rescale_ts(0, rational, rational)],
            [AC_MSG_RESULT([yes])
             AC_DEFINE(HAVE_AV_PACKET_RESCALE_TS,1,[av_packet_rescale_ts available])],
            [AC_MSG_RESULT([no])])
AC_MSG_CHECKING([for av_packet_unref])
AC_TRY_LINK([#include <libavcodec/avcodec.h>],
            [av_packet_unref(NULL)],
            [AC_MSG_RESULT([yes])
             AC_DEFINE(HAVE_AV_PACKET_UNREF,1,[av_packet_unref available])],
            [AC_MSG_RESULT([no])])
LIBS="$old_LIBS"

LIBS="$LIBS -lXpm"
AC_MSG_CHECKING([for XpmCreatePixmapFromData])
AC_TRY_LINK([#include <X11/xpm.h>],
            [XpmCreatePixmapFromData(0, 0, 0, 0, 0, 0)],
            [AC_MSG_RESULT([yes])],
            [AC_MSG_RESULT([no])]
            [AC_MSG_ERROR([Could not locate Xpm library])])
LIBS="$old_LIBS"

GUILE_PKG([2.2 2.0])
GUILE_PROGS
GUILE_FLAGS
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
old_LIBS="$LIBS"

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
          tests/integration/Makefile
          tests/fixtures/Makefile
          srfi/Makefile
          srfi/srfi-64/Makefile
          bench/Makefile
          doc/Makefile
          doc/index.md,
          echo "$info",
          info="$info")
