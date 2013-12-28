SUFFIXES = .c .scm .go .nw

sitedir = $(GUILE_SITE)/math
site_DATA = aiscm.scm

extensiondir = $(GUILE_EXT)
extension_LTLIBRARIES = libguile-aiscm.la

cachedir = $(GUILE_CACHE)/math
cache_DATA = aiscm.go

libguile_aiscm_la_SOURCES = aiscm.c
libguile_aiscm_la_CFLAGS = $(GUILE_CFLAGS)
libguile_aiscm_la_LDFLAGS = -no-undefined
libguile_aiscm_la_LIBADD = $(GUILE_LIBS)

install-data-hook:
	$(TOUCH) $(cachedir)/aiscm.go

.scm.go:
	$(GUILE_TOOLS) compile $< -o $@

EXTRA_DIST = aiscm.scm

CLEANFILES = *~ *.tmp *.go

DISTCLEANFILES = .*.un~ .*.swp

MAINTAINERCLEANFILES = Makefile.in
