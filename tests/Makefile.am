SUFFIXES = .scm .log .trs

SUBDIRS = integration fixtures

TEST_EXTENSIONS = .scm
TESTS_ENVIRONMENT = LD_LIBRARY_PATH=$(top_builddir)/aiscm/.libs:$(LD_LIBRARY_PATH) GUILE_AUTO_COMPILE=0
SCM_LOG_COMPILE = $(GUILE) -L $(top_builddir) -L $(srcdir)
SCM_LOG_DRIVER = env AM_TAP_AWK='$(AWK)' $(SHELL) $(top_srcdir)/build-aux/tap-driver.sh --comments

TESTS = test_mem.scm test_bool.scm test_int.scm test_float.scm test_rgb.scm test_pointer.scm \
				test_sequence.scm test_asm.scm test_jit1.scm test_jit2.scm test_jit3.scm test_jit4.scm test_jit5.scm test_jit6.scm \
				test_op.scm test_util.scm test_image.scm test_complex.scm test_magick.scm test_pulse.scm test_ffmpeg.scm \
				test_ringbuffer.scm

EXTRA_DIST = guile-tap.scm $(TESTS)

CLEANFILES = *~ *.tmp *.log *.trs

MAINTAINERCLEANFILES = Makefile.in
