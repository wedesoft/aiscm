ENVIRONMENT = LD_LIBRARY_PATH=$(top_builddir)/aiscm/.libs:$(LD_LIBRARY_PATH) GUILE_AUTO_COMPILE=0
RUN = $(GUILE) -L $(top_builddir) -L $(top_builddir)/tests

SUBDIRS = aiscm tests doc bench srfi

ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST = bootstrap configure.ac README README.md TODO.md COPYING NEWS NEWS.md ChangeLog AUTHORS

repl:
	$(ENVIRONMENT) GC_INITIAL_HEAP_SIZE=1G GC_USE_ENTIRE_HEAP=Y DEBUG=YES $(RUN)

CLEANFILES = *~ *.html

MAINTAINERCLEANFILES = aclocal.m4 configure Makefile.in mkinstalldirs \
	stamp-h.in libtool config.cache config.h config.h.in acinclude.m4 \
	m4/libtool.m4 m4/lt~obsolete.m4 m4/ltoptions.m4 m4/ltsugar.m4 m4/ltversion.m4

maintainer-clean-local:
	rm -Rf build-aux pkg
