SUFFIXES = .c .scm .go

sitedir = $(GUILE_SITE)/aiscm
site_DATA = malloc.scm

extensiondir = $(GUILE_EXT)
extension_LTLIBRARIES = libguile-malloc.la

cachedir = $(GUILE_CACHE)/aiscm
cache_DATA = malloc.go

libguile_malloc_la_SOURCES = malloc.c
libguile_malloc_la_CFLAGS = $(GUILE_CFLAGS)
libguile_malloc_la_LDFLAGS = -no-undefined
libguile_malloc_la_LIBADD = $(GUILE_LIBS)

install-data-hook:
	$(TOUCH) $(cachedir)/malloc.go

.scm.go:
	$(GUILE_TOOLS) compile $< -o $@

EXTRA_DIST = malloc.scm

CLEANFILES = *~ *.tmp *.go

DISTCLEANFILES = .*.un~ .*.swp

MAINTAINERCLEANFILES = Makefile.in
