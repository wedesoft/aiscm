doc_DATA = index.html installation.html assembler.html io.html aiscm.gif

EXTRA_DIST = includes.scm index.md.in installation.md.in assembler.md io.md aiscm.gif

CLEANFILES = *~ *.html

MAINTAINERCLEANFILES = Makefile.in index.md installation.md

.md.html:
	$(GUILE) includes.scm < $< | $(PANDOC) -s -f markdown -t html > $@
