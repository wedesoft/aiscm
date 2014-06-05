doc_DATA = aiscm.html intro.html construction.gif

EXTRA_DIST = aiscm.md intro.md.in construction.gif

CLEANFILES = *~ *.html

MAINTAINERCLEANFILES = Makefile.in intro.md

.md.html:
	$(PANDOC) -s $< > $@
