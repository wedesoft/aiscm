SUFFIXES = .md .html

SUBDIRS = aiscm tests doc

ACLOCAL_AMFLAGS = -I m4

noinst_DATA = README.html TODO.html

EXTRA_DIST = Makefile.dist configure.ac README.md TODO.md COPYING \
	repl.sh construction.gif

.md.html:
	$(PANDOC) -o $@ $< || echo "Install Pandoc to compile Markdown to HTML"

CLEANFILES = *~ *.html

DISTCLEANFILES = .*.un~ .*.swp

MAINTAINERCLEANFILES = aclocal.m4 configure \
	Makefile.in mkinstalldirs stamp-h.in \
	libtool config.cache config.h config.h.in acinclude.m4 \
	m4/libtool.m4 m4/lt~obsolete.m4 m4/ltoptions.m4 m4/ltsugar.m4 m4/ltversion.m4
