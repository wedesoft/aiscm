SUFFIXES = .c .scm .go

sitedir = $(GUILE_SITE)/aiscm
site_DATA = mem.scm element.scm scalar.scm bool.scm int.scm pointer.scm rgb.scm complex.scm float.scm \
						sequence.scm asm.scm image.scm jit.scm op.scm ffmpeg.scm util.scm magick.scm \
						pulse.scm v4l2.scm xorg.scm obj.scm composite.scm

extensiondir = $(GUILE_EXT)
extension_LTLIBRARIES = libguile-aiscm-util.la libguile-aiscm-jit.la libguile-aiscm-ffmpeg.la libguile-aiscm-image.la \
												libguile-aiscm-magick.la libguile-aiscm-pulse.la libguile-aiscm-v4l2.la libguile-aiscm-xorg.la

check_LTLIBRARIES = libguile-aiscm-tests.la

noinst_HEADERS = ffmpeg-helpers.h ringbuffer.h

cachedir = $(GUILE_CACHE)/aiscm
cache_DATA = $(site_DATA:.scm=.go)

libguile_aiscm_tests_la_SOURCES = tests.c ffmpeg-helpers.c ffmpeg-helpers-tests.c ringbuffer.c ringbuffer-tests.c jit-tests.c
libguile_aiscm_tests_la_CFLAGS = $(GUILE_CFLAGS)
libguile_aiscm_tests_la_LDFLAGS = -no-undefined -module -shared -avoid-version -rpath /tmp
libguile_aiscm_tests_la_LIBADD = $(GUILE_LIBS)

libguile_aiscm_util_la_SOURCES = util.c
libguile_aiscm_util_la_CFLAGS = $(GUILE_CFLAGS)
libguile_aiscm_util_la_LDFLAGS = -no-undefined
libguile_aiscm_util_la_LIBADD = $(GUILE_LIBS)

libguile_aiscm_jit_la_SOURCES = jit.c
libguile_aiscm_jit_la_CFLAGS = $(GUILE_CFLAGS)
libguile_aiscm_jit_la_LDFLAGS = -no-undefined
libguile_aiscm_jit_la_LIBADD = $(GUILE_LIBS)

libguile_aiscm_ffmpeg_la_SOURCES = ffmpeg.c ffmpeg-helpers.c
libguile_aiscm_ffmpeg_la_CFLAGS = $(GUILE_CFLAGS) $(AVFORMAT_CFLAGS) $(AVCODEC_CFLAGS) $(AVUTIL_CFLAGS)
libguile_aiscm_ffmpeg_la_LDFLAGS = -no-undefined
libguile_aiscm_ffmpeg_la_LIBADD = $(GUILE_LIBS) $(AVFORMAT_LIBS) $(AVCODEC_LIBS) $(AVUTIL_LIBS)

libguile_aiscm_image_la_SOURCES = image.c
libguile_aiscm_image_la_CFLAGS = $(GUILE_CFLAGS) $(SWSCALE_CFLAGS) $(MJPEG_CFLAGS)
libguile_aiscm_image_la_LDFLAGS = -no-undefined
libguile_aiscm_image_la_LIBADD = $(GUILE_LIBS) $(SWSCALE_LIBS) $(MJPEG_LIBS) -llavjpeg

libguile_aiscm_magick_la_SOURCES = magick.c
libguile_aiscm_magick_la_CFLAGS = $(GUILE_CFLAGS) $(MAGICK_CFLAGS)
libguile_aiscm_magick_la_LDFLAGS = -no-undefined
libguile_aiscm_magick_la_LIBADD = $(GUILE_LIBS) $(MAGICK_LIBS)

libguile_aiscm_pulse_la_SOURCES = pulse.c ringbuffer.c
libguile_aiscm_pulse_la_CFLAGS = $(GUILE_CFLAGS) $(PULSE_CFLAGS) $(PTHREAD_CFLAGS)
libguile_aiscm_pulse_la_LDFLAGS = -no-undefined
libguile_aiscm_pulse_la_LIBADD = $(GUILE_LIBS) $(PULSE_LIBS) $(PTHREAD_LIBS)

libguile_aiscm_v4l2_la_SOURCES = v4l2.c
libguile_aiscm_v4l2_la_CFLAGS = $(GUILE_CFLAGS)
libguile_aiscm_v4l2_la_LDFLAGS = -no-undefined
libguile_aiscm_v4l2_la_LIBADD = $(GUILE_LIBS)

libguile_aiscm_xorg_la_SOURCES = xorg.c
libguile_aiscm_xorg_la_CFLAGS = $(GUILE_CFLAGS) $(X_CFLAGS)
libguile_aiscm_xorg_la_LDFLAGS = -no-undefined
libguile_aiscm_xorg_la_LIBADD = $(GUILE_LIBS) $(GL_LIBS) $(X_PRE_LIBS) $(X_LIBS) -lX11 $(X_EXTRA_LIBS) -lXv -lXpm

install-data-hook:
	$(TOUCH) $(addprefix $(DESTDIR)$(cachedir)/,$(cache_DATA))

.scm.go:
	LD_LIBRARY_PATH=$(srcdir)/.libs:$(LD_LIBRARY_PATH) GUILE_AUTO_COMPILE=0 $(GUILE_TOOLS) compile -L$(top_builddir) -o $@ $<

$(cache_DATA): $(extension_LTLIBRARIES)

EXTRA_DIST = $(site_DATA) aiscm.xpm

CLEANFILES = *.tmp *.go*

MAINTAINERCLEANFILES = Makefile.in

maintainer-clean-local:
	rm -Rf .deps
