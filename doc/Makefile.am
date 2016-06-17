SUFFIXES = .scm .png .jpg .md .html .tmp .css .html5

PANDOC_OPTS = -s -c buttondown.css -f markdown -t html
PANDOC_VARS = -Vlang=en -Vauthor-meta="Jan Wedekind" -Vdate-meta=$(shell date --iso-8601) -Vpagetitle=AIscm

ENVIRONMENT = LD_LIBRARY_PATH=$(top_builddir)/aiscm/.libs:$(LD_LIBRARY_PATH)
RUN = $(GUILE) --no-auto-compile -L $(top_builddir)

doc_DATA = index.html installation.html assembler.html io.html array.html aiscm.gif fubk.png scaled-pattern.png \
	pavillion.jpg swap-channels.jpg

EXTRA_DIST = includes.scm buttondown.css aiscm.html5 index.md.in installation.md assembler.md io.md array.md aiscm.gif

CLEANFILES = fubk.png pavillion.jpg swap-channels.jpg pattern.png scaled-pattern.png *.html *.tmp

MAINTAINERCLEANFILES = Makefile.in index.md

fubk.png: ../tests/integration/fubk.png
	cp $< $@

pavillion.jpg: ../tests/integration/pavillion.jpg
	cp $< $@

pattern.png: ../tests/integration/write_image.scm
	$(ENVIRONMENT) $(RUN) $<

scaled-pattern.png: pattern.png
	$(CONVERT) $< -scale 320 $@

swap-channels.jpg: ../tests/integration/swap_channels.scm pavillion.jpg
	$(ENVIRONMENT) $(RUN) $<

.md.tmp:
	$(GUILE) includes.scm < $< > $@ || rm -f $@

%.html: %.tmp buttondown.css
	$(PANDOC) $(PANDOC_OPTS) $(PANDOC_VARS) -o $@ $<
