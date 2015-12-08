SUFFIXES = .c .scm .go

sitedir = $(GUILE_SITE)/aiscm
site_DATA = mem.scm element.scm bool.scm int.scm pointer.scm rgb.scm complex.scm float.scm sequence.scm \
	asm.scm jit.scm op.scm compile.scm util.scm image.scm v4l2.scm xorg.scm

extensiondir = $(GUILE_EXT)
extension_LTLIBRARIES = libguile-util.la libguile-jit.la libguile-image.la libguile-v4l2.la libguile-xorg.la

cachedir = $(GUILE_CACHE)/aiscm
cache_DATA = $(site_DATA:.scm=.go)

libguile_util_la_SOURCES = util.c
libguile_util_la_CFLAGS = $(GUILE_CFLAGS)
libguile_util_la_LDFLAGS = -no-undefined
libguile_util_la_LIBADD = $(GUILE_LIBS)

libguile_jit_la_SOURCES = jit.c
libguile_jit_la_CFLAGS = $(GUILE_CFLAGS)
libguile_jit_la_LDFLAGS = -no-undefined
libguile_jit_la_LIBADD = $(GUILE_LIBS)

libguile_image_la_SOURCES = image.c
libguile_image_la_CFLAGS = $(GUILE_CFLAGS) $(SWSCALE_CFLAGS) $(MJPEG_CFLAGS)
libguile_image_la_LDFLAGS = -no-undefined
libguile_image_la_LIBADD = $(GUILE_LIBS) $(SWSCALE_LIBS) $(MJPEG_LIBS) -llavjpeg

libguile_v4l2_la_SOURCES = v4l2.c
libguile_v4l2_la_CFLAGS = $(GUILE_CFLAGS)
libguile_v4l2_la_LDFLAGS = -no-undefined
libguile_v4l2_la_LIBADD = $(GUILE_LIBS)

libguile_xorg_la_SOURCES = xorg.c
libguile_xorg_la_CFLAGS = $(GUILE_CFLAGS) $(X_CFLAGS)
libguile_xorg_la_LDFLAGS = -no-undefined
libguile_xorg_la_LIBADD = $(GUILE_LIBS) $(GL_LIBS) $(X_PRE_LIBS) $(X_LIBS) -lX11 $(X_EXTRA_LIBS) -lXv

install-data-hook:
	$(TOUCH) $(addprefix $(DESTDIR)$(cachedir)/,$(cache_DATA))

.scm.go:
	LD_LIBRARY_PATH=$(srcdir)/.libs:$(LD_LIBRARY_PATH) $(GUILE_TOOLS) compile $< -L$(top_builddir) -o $@

$(cache_DATA): $(extension_LTLIBRARIES)

EXTRA_DIST = $(site_DATA)

CLEANFILES = *~ *.tmp *.go*

DISTCLEANFILES = .*.un~ .*.swp

MAINTAINERCLEANFILES = Makefile.in
