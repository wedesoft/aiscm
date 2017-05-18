SUFFIXES = .scm .log .trs

SUBDIRS = integration fixtures

TEST_EXTENSIONS = .scm
TESTS_ENVIRONMENT = LD_LIBRARY_PATH=$(top_builddir)/aiscm/.libs:$(LD_LIBRARY_PATH) GUILE_AUTO_COMPILE=0
SCM_LOG_DRIVER = $(GUILE) -L $(top_builddir) -L $(srcdir) $(top_srcdir)/build-aux/test-driver.scm

TESTS = test_asm.scm test_bool.scm test_complex.scm test_ffmpeg_in.scm test_ffmpeg_out.scm test_float.scm test_image.scm \
				test_int.scm test_jit1.scm test_jit2.scm test_jit3.scm test_jit4.scm test_jit5.scm \
				test_magick.scm test_mem.scm test_method.scm test_obj.scm test_pointer.scm \
				test_pulse.scm test_rgb.scm test_ringbuffer.scm test_sequence.scm test_util.scm test_samples.scm \
				test_tensor.scm test_variable.scm test_command.scm test_program.scm test_live_analysis.scm test_register_allocate.scm \
				test_compile.scm test_expression.scm test_loop.scm

EXTRA_DIST = $(TESTS)

CLEANFILES = *~ *.tmp *.log *.trs

MAINTAINERCLEANFILES = Makefile.in
