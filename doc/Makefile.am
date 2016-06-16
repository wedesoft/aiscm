SUFFIXES = .scm .png .md .html

ENVIRONMENT = LD_LIBRARY_PATH=$(top_builddir)/aiscm/.libs:$(LD_LIBRARY_PATH)
RUN = $(GUILE) --no-auto-compile -L $(top_builddir)

doc_DATA = index.html installation.html assembler.html io.html array.html aiscm.gif fubk.png scaled-pattern.png \
					 swap-channels.png

EXTRA_DIST = includes.scm buttondown.css index.md.in installation.md.in assembler.md io.md array.md aiscm.gif

CLEANFILES = *~ *.html

MAINTAINERCLEANFILES = Makefile.in index.md installation.md

fubk.png: ../tests/fubk.png
	cp $< $@

pattern.png: ../tests/integration_write_image.scm
	$(ENVIRONMENT) $(RUN) $<

scaled-pattern.png: pattern.png
	$(CONVERT) $< -scale 320 $@

swap-channels.png: ../tests/integration_swap_channels.scm
	$(ENVIRONMENT) $(RUN) $<

.md.html:
	$(GUILE) includes.scm < $< | $(PANDOC) -s -c buttondown.css -f markdown -t html > $@
