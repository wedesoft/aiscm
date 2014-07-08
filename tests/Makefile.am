TEST_EXTENSIONS = .scm
TESTS_ENVIRONMENT = LD_LIBRARY_PATH=$(top_builddir)/aiscm/.libs:$(LD_LIBRARY_PATH)
SCM_LOG_COMPILE = $(GUILE) --no-auto-compile -L $(top_builddir) -L $(srcdir)
SCM_LOG_DRIVER = env AM_TAP_AWK='$(AWK)' $(SHELL) $(top_srcdir)/build-aux/tap-driver.sh

TESTS = test_mem.scm test_bool.scm test_int.scm test_pointer.scm \
				test_sequence.scm test_jit.scm test_op.scm test_util.scm test_compile.scm

EXTRA_DIST = guile-tap.scm $(TESTS)

CLEANFILES = *~ *.tmp *.log *.trs

DISTCLEANFILES = .*.un~ .*.swp

MAINTAINERCLEANFILES = Makefile.in
