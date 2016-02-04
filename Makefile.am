SUBDIRS = aiscm tests doc bench

ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST = Makefile.dist configure.ac README.md TODO.md COPYING

repl: all
	LD_LIBRARY_PATH=$(top_builddir)/aiscm/.libs:$(LD_LIBRARY_PATH) GC_INITIAL_HEAP_SIZE=1G GC_USE_ENTIRE_HEAP=Y \
  $(GUILE) --no-auto-compile -L $(srcdir)

integration: all
	cd tests && $(MAKE) $@ && cd ..

CLEANFILES = *~ *.html

DISTCLEANFILES = .*.un~ .*.swp

MAINTAINERCLEANFILES = aclocal.m4 configure Makefile.in mkinstalldirs \
	stamp-h.in libtool config.cache config.h config.h.in acinclude.m4 \
	m4/libtool.m4 m4/lt~obsolete.m4 m4/ltoptions.m4 m4/ltsugar.m4 m4/ltversion.m4

maintainer-clean-local:
	rm -Rf build-aux pkg
