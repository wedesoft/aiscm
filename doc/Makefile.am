doc_DATA = index.html installation.html construction.gif

EXTRA_DIST = index.md installation.md.in construction.gif

CLEANFILES = *~ *.html

MAINTAINERCLEANFILES = Makefile.in installation.md

.md.html:
	$(PANDOC) -s -f markdown -t html $< > $@
