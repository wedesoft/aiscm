SUFFIXES = .c .scm .go

sitedir = $(GUILE_SITE)/aiscm
site_SCRIPTS = mem.scm element.scm bool.scm int.scm pointer.scm \
							 var.scm lookup.scm lambda.scm sequence.scm \
							 elementwise.scm jit.scm compile.scm

extensiondir = $(GUILE_EXT)
extension_LTLIBRARIES = libguile-mem.la libguile-jit.la

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

install-data-hook:
	$(TOUCH) $(addprefix $(cachedir)/,$(cache_DATA))

.scm.go:
	LD_LIBRARY_PATH=$(srcdir)/.libs:$(LD_LIBRARY_PATH) $(GUILE_TOOLS) compile $< -L$(top_builddir) -o $@

EXTRA_DIST = $(site_SCRIPTS)

CLEANFILES = *~ *.tmp *.go

DISTCLEANFILES = .*.un~ .*.swp

MAINTAINERCLEANFILES = Makefile.in
