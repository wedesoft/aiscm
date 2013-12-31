TESTS_ENVIRONMENT = \
  LD_LIBRARY_PATH=$(top_builddir)/aiscm/.libs:$(LD_LIBRARY_PATH) \
	$(GUILE) --no-auto-compile -L $(top_builddir)
TESTS = test_malloc.scm

EXTRA_DIST = $(TESTS)

CLEANFILES = *~ *.tmp

DISTCLEANFILES = .*.un~ .*.swp

MAINTAINERCLEANFILES = Makefile.in
