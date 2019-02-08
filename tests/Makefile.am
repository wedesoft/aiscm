SUFFIXES = .scm .log .trs

SUBDIRS = integration fixtures

TEST_EXTENSIONS = .scm
TESTS_ENVIRONMENT = LD_LIBRARY_PATH=$(top_builddir)/aiscm/.libs:$(LD_LIBRARY_PATH) GUILE_AUTO_COMPILE=0
SCM_LOG_DRIVER = $(GUILE) -L $(top_builddir) -L $(srcdir) $(top_srcdir)/build-aux/test-driver.scm

TESTS = test_core.scm test_tensors.scm test_ffmpeg_in.scm test_ffmpeg_out.scm test_image.scm test_magick.scm \
				test_pulse.scm test_ringbuffer.scm test_util.scm test_samples.scm test_xorg.scm \
				test_tensorflow.scm test_filters.scm test_opencv.scm

EXTRA_DIST = $(TESTS)

CLEANFILES = *~ *.log *.trs

MAINTAINERCLEANFILES = Makefile.in
