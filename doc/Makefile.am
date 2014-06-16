doc_DATA = index.html installation.html native.html construction.gif

EXTRA_DIST = index.md.in installation.md.in native.md construction.gif

CLEANFILES = *~ *.html

MAINTAINERCLEANFILES = Makefile.in index.md installation.md

.md.html:
	$(PANDOC) -s -f markdown -t html $< > $@
