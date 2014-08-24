SUFFIXES = .c .scm .go

sitedir = $(GUILE_SITE)/aiscm
site_SCRIPTS = mem.scm element.scm bool.scm int.scm pointer.scm sequence.scm \
	op.scm jit.scm compile.scm util.scm v4l2.scm


extensiondir = $(GUILE_EXT)
extension_LTLIBRARIES = libguile-mem.la libguile-jit.la libguile-v4l2.la

cachedir = $(GUILE_CACHE)/aiscm
cache_DATA = $(site_SCRIPTS:.scm=.go)

libguile_mem_la_SOURCES = mem.c
libguile_mem_la_CFLAGS = $(GUILE_CFLAGS)
libguile_mem_la_LDFLAGS = -no-undefined
libguile_mem_la_LIBADD = $(GUILE_LIBS)

libguile_jit_la_SOURCES = jit.c
libguile_jit_la_CFLAGS = $(GUILE_CFLAGS)
libguile_jit_la_LDFLAGS = -no-undefined
libguile_jit_la_LIBADD = $(GUILE_LIBS)

libguile_v4l2_la_SOURCES = v4l2.c
libguile_v4l2_la_CFLAGS = $(GUILE_CFLAGS)
libguile_v4l2_la_LDFLAGS = -no-undefined
libguile_v4l2_la_LIBADD = $(GUILE_LIBS)

install-data-hook:
	$(TOUCH) $(addprefix $(cachedir)/,$(cache_DATA))

.scm.go:
	LD_LIBRARY_PATH=$(srcdir)/.libs:$(LD_LIBRARY_PATH) $(GUILE_TOOLS) compile $< -L$(top_builddir) -o $@

EXTRA_DIST = $(site_SCRIPTS)

CLEANFILES = *~ *.tmp *.go

DISTCLEANFILES = .*.un~ .*.swp

MAINTAINERCLEANFILES = Makefile.in
