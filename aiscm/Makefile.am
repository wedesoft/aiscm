SUFFIXES = .c .scm .go .nw

sitedir = $(GUILE_SITE)/math
site_DATA = bessel.scm

extensiondir = $(GUILE_EXT)
extension_LTLIBRARIES = libguile-bessel.la

cachedir = $(GUILE_CACHE)
cache_DATA = bessel.scm.go

libguile_bessel_la_SOURCES = bessel.c
libguile_bessel_la_CFLAGS = $(GUILE_CFLAGS)
libguile_bessel_la_LDFLAGS =
libguile_bessel_la_LIBADD = $(GUILE_LIBS)

%.scm.go: %.scm
	$(GUILE_TOOLS) compile $< -o $@

EXTRA_DIST = bessel.scm

CLEANFILES = *~ *.tmp

DISTCLEANFILES = .*.un~ .*.swp

MAINTAINERCLEANFILES = Makefile.in
