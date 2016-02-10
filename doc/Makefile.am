ENVIRONMENT = LD_LIBRARY_PATH=$(top_builddir)/aiscm/.libs:$(LD_LIBRARY_PATH)
RUN = $(GUILE) --no-auto-compile -L $(top_builddir)

doc_DATA = index.html installation.html assembler.html io.html aiscm.gif fubk.png scaled-pattern.png

EXTRA_DIST = includes.scm buttondown.css index.md.in installation.md.in assembler.md io.md aiscm.gif

CLEANFILES = *~ *.html

MAINTAINERCLEANFILES = Makefile.in index.md installation.md

fubk.png: ../tests/fubk.png
	cp $< $@

pattern.png: ../tests/integration_write_image.scm
	$(ENVIRONMENT) $(RUN) $<

scaled-pattern.png: pattern.png
	$(CONVERT) $< -scale 320 $@

.md.html:
	$(GUILE) includes.scm < $< | $(PANDOC) -s -c buttondown.css -f markdown -t html > $@
