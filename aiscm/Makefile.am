SUFFIXES = .c .scm .go .in

sitedir = $(GUILE_SITE)/aiscm
site_DATA = core.scm tensors.scm image.scm ffmpeg.scm util.scm magick.scm pulse.scm v4l2.scm xorg.scm samples.scm \
						tensorflow.scm

extensiondir = $(GUILE_EXT)
extension_LTLIBRARIES = libguile-aiscm-util.la libguile-aiscm-ffmpeg.la libguile-aiscm-image.la \
												libguile-aiscm-magick.la libguile-aiscm-pulse.la libguile-aiscm-v4l2.la libguile-aiscm-xorg.la \
												libguile-aiscm-samples.la libguile-aiscm-core.la \
												libguile-aiscm-tensorflow.la

check_LTLIBRARIES = libguile-aiscm-tests.la

noinst_HEADERS = ffmpeg-helpers.h util-helpers.h ringbuffer.h samples-helpers.h

noinst_PROGRAMS = tensorflow-generate

BUILT_SOURCES = tensorflow.scm

cachedir = $(GUILE_CACHE)/aiscm
cache_DATA = $(site_DATA:.scm=.go)

EXTRA_DIST = $(site_DATA) aiscm.xpm tensorflow.scm.in

AISCM_LDFLAGS = -no-undefined -module -shared -avoid-version

tensorflow_generate_SOURCES = tensorflow-generate.c
tensorflow_generate_CFLAGS = -I/usr/local/include/ClearSilver -I/usr/include/ClearSilver
tensorflow_generate_LDFLAGS =
tensorflow_generate_LDADD = ../tensorflow/core/framework/libtensorflow-ops.la -lneo_cs -lneo_utl -ltensorflow -lgc

libguile_aiscm_tests_la_SOURCES = tests.c ffmpeg-helpers.c ffmpeg-helpers-tests.c ringbuffer.c ringbuffer-tests.c \
																	util-helpers.c util-helpers-tests.c samples-helpers.c samples-helpers-tests.c
libguile_aiscm_tests_la_CFLAGS = $(GUILE_CFLAGS)
libguile_aiscm_tests_la_LDFLAGS = -no-undefined -module -shared -avoid-version -rpath /tmp
libguile_aiscm_tests_la_LIBADD = $(GUILE_LIBS)

libguile_aiscm_util_la_SOURCES = util.c
libguile_aiscm_util_la_CFLAGS = $(GUILE_CFLAGS)
libguile_aiscm_util_la_LDFLAGS = $(AISCM_LDFLAGS)
libguile_aiscm_util_la_LIBADD = $(GUILE_LIBS)

libguile_aiscm_ffmpeg_la_SOURCES = ffmpeg.c ffmpeg-helpers.c ringbuffer.c util-helpers.c
libguile_aiscm_ffmpeg_la_CFLAGS = $(GUILE_CFLAGS) $(AVFORMAT_CFLAGS) $(AVCODEC_CFLAGS) $(AVUTIL_CFLAGS)
libguile_aiscm_ffmpeg_la_LDFLAGS = $(AISCM_LDFLAGS)
libguile_aiscm_ffmpeg_la_LIBADD = $(GUILE_LIBS) $(AVFORMAT_LIBS) $(AVCODEC_LIBS) $(AVUTIL_LIBS)

libguile_aiscm_image_la_SOURCES = image.c util-helpers.c
libguile_aiscm_image_la_CFLAGS = $(GUILE_CFLAGS) $(SWSCALE_CFLAGS) $(MJPEG_CFLAGS)
libguile_aiscm_image_la_LDFLAGS = $(AISCM_LDFLAGS)
libguile_aiscm_image_la_LIBADD = $(GUILE_LIBS) $(SWSCALE_LIBS) $(MJPEG_LIBS) -llavjpeg

libguile_aiscm_magick_la_SOURCES = magick.c
libguile_aiscm_magick_la_CFLAGS = $(GUILE_CFLAGS) $(MAGICK_CFLAGS)
libguile_aiscm_magick_la_LDFLAGS = $(AISCM_LDFLAGS)
libguile_aiscm_magick_la_LIBADD = $(GUILE_LIBS) $(MAGICK_LIBS)

libguile_aiscm_pulse_la_SOURCES = pulse.c ringbuffer.c
libguile_aiscm_pulse_la_CFLAGS = $(GUILE_CFLAGS) $(PULSE_CFLAGS) $(PTHREAD_CFLAGS)
libguile_aiscm_pulse_la_LDFLAGS = $(AISCM_LDFLAGS)
libguile_aiscm_pulse_la_LIBADD = $(GUILE_LIBS) $(PULSE_LIBS) $(PTHREAD_LIBS)

libguile_aiscm_v4l2_la_SOURCES = v4l2.c util-helpers.c
libguile_aiscm_v4l2_la_CFLAGS = $(GUILE_CFLAGS)
libguile_aiscm_v4l2_la_LDFLAGS = $(AISCM_LDFLAGS)
libguile_aiscm_v4l2_la_LIBADD = $(GUILE_LIBS)

libguile_aiscm_xorg_la_SOURCES = xorg.c
libguile_aiscm_xorg_la_CFLAGS = $(GUILE_CFLAGS) $(X_CFLAGS)
libguile_aiscm_xorg_la_LDFLAGS = $(AISCM_LDFLAGS)
libguile_aiscm_xorg_la_LIBADD = $(GUILE_LIBS) $(GL_LIBS) $(X_PRE_LIBS) $(X_LIBS) -lX11 $(X_EXTRA_LIBS) -lXv -lXpm

libguile_aiscm_samples_la_SOURCES = samples.c samples-helpers.c util-helpers.c
libguile_aiscm_samples_la_CFLAGS = $(GUILE_CFLAGS) $(SWRESAMPLE_CFLAGS) $(AVRESAMPLE_CFLAGS) $(AVUTIL_CFLAGS)
libguile_aiscm_samples_la_LDFLAGS = $(AISCM_LDFLAGS)
libguile_aiscm_samples_la_LIBADD = $(GUILE_LIBS) $(SWRESAMPLE_LIBS) $(AVRESAMPLE_LIBS) $(AVUTIL_LIBS)

libguile_aiscm_core_la_SOURCES = core.c
libguile_aiscm_core_la_CFLAGS = $(GUILE_CFLAGS) $(LLVM_CFLAGS)
libguile_aiscm_core_la_LDFLAGS = $(AISCM_LDFLAGS) $(LLVM_LDFLAGS)
libguile_aiscm_core_la_LIBADD = $(GUILE_LIBS) $(LLVM_LIBS)

libguile_aiscm_tensorflow_la_SOURCES = tensorflow.c
libguile_aiscm_tensorflow_la_CFLAGS = $(GUILE_CFLAGS)
libguile_aiscm_tensorflow_la_LDFLAGS = $(AISCM_LDFLAGS)
libguile_aiscm_tensorflow_la_LIBADD = $(GUILE_LIBS) -ltensorflow

CLEANFILES = *.tmp *.go*

MAINTAINERCLEANFILES = Makefile.in

install-data-hook:
	$(TOUCH) $(addprefix $(DESTDIR)$(cachedir)/,$(cache_DATA))

.scm.go:
	LD_LIBRARY_PATH=$(srcdir)/.libs:$(LD_LIBRARY_PATH) GUILE_AUTO_COMPILE=0 $(GUILE_TOOLS) compile -L$(top_builddir) -o $@ $<

tensorflow.scm: tensorflow.scm.in tensorflow-generate
	./tensorflow-generate $< > $@

maintainer-clean-local:
	rm -Rf .deps

$(cache_DATA): $(extension_LTLIBRARIES)
