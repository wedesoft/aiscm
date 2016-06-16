SUFFIXES = .scm .tmp

SUBDIRS = fixtures

TEST_EXTENSIONS = .scm
TESTS_ENVIRONMENT = LD_LIBRARY_PATH=$(top_builddir)/aiscm/.libs:$(LD_LIBRARY_PATH)
SCM_LOG_COMPILE = $(GUILE) --no-auto-compile -L $(top_builddir) -L $(srcdir)
SCM_LOG_DRIVER = env AM_TAP_AWK='$(AWK)' $(SHELL) $(top_srcdir)/build-aux/tap-driver.sh --comments
INTEGRATION_RUN = $(TESTS_ENVIRONMENT) $(SCM_LOG_COMPILE)

TESTS = test_mem.scm test_bool.scm test_int.scm test_float.scm test_rgb.scm test_pointer.scm \
				test_sequence.scm test_asm.scm test_jit1.scm test_jit2.scm test_jit3.scm test_jit4.scm test_jit5.scm \
				test_op.scm test_util.scm test_compile.scm test_image.scm test_complex.scm \
				test_magick.scm test_pulse.scm

INTEGRATION_TESTS = integration_grab.scm integration_camera_mode.scm \
										integration_xorg_image_list.scm integration_xorg_video.scm \
										integration_xorg_video_list.scm integration_xorg_window.scm \
										integration_pulse_out.scm integration_pulse_in.scm integration_asm.scm \
										integration_jmp.scm integration_virtual_registers.scm integration_virtual_arrays.scm \
										integration_read_image.scm integration_write_image.scm integration_swap_channels.scm

INTEGRATION_TEST_TARGETS = $(addsuffix .tmp, $(basename $(INTEGRATION_TESTS)))

.scm.tmp:
	@echo Running integration test $< ...
	$(INTEGRATION_RUN) $<
	$(TOUCH) $@
	@echo done.

integration: $(INTEGRATION_TEST_TARGETS)

EXTRA_DIST = guile-tap.scm $(TESTS) $(INTEGRATION_TESTS) fubk.png pavillion.jpg

CLEANFILES = *~ *.tmp *.log *.trs

DISTCLEANFILES = .*.un~ .*.swp

MAINTAINERCLEANFILES = Makefile.in
