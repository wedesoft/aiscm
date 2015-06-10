SUBDIRS = aiscm tests bench doc

ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST = Makefile.dist configure.ac README.md TODO.md COPYING construction.gif

repl: all
	LD_LIBRARY_PATH=$(top_builddir)/aiscm/.libs:$(LD_LIBRARY_PATH) \
  $(GUILE) --no-auto-compile -L $(top_builddir)

CLEANFILES = *~ *.html

DISTCLEANFILES = .*.un~ .*.swp

MAINTAINERCLEANFILES = aclocal.m4 configure Makefile.in mkinstalldirs \
	stamp-h.in libtool config.cache config.h config.h.in acinclude.m4 \
	m4/libtool.m4 m4/lt~obsolete.m4 m4/ltoptions.m4 m4/ltsugar.m4 m4/ltversion.m4

maintainer-clean-local:
	rm -Rf build-aux
