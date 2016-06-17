SUFFIXES = .scm .tmp

TESTS_ENVIRONMENT = LD_LIBRARY_PATH=$(top_builddir)/aiscm/.libs:$(LD_LIBRARY_PATH)
SCM_LOG_COMPILE = $(GUILE) --no-auto-compile -L $(top_builddir) -L $(srcdir)
INTEGRATION_RUN = $(TESTS_ENVIRONMENT) $(SCM_LOG_COMPILE)

INTEGRATION_TESTS = grab.scm camera_mode.scm xorg_image_list.scm xorg_video.scm \
										xorg_video_list.scm xorg_window.scm pulse_out.scm pulse_in.scm asm.scm \
										jmp.scm virtual_registers.scm virtual_arrays.scm \
										read_image.scm write_image.scm swap_channels.scm \
										byte_sequence.scm type_matching.scm 2d_array.scm 2d_matching.scm \
										typed_sequence.scm integer_type.scm

EXTRA_DIST = $(INTEGRATION_TESTS) fubk.png pavillion.jpg

INTEGRATION_TEST_TARGETS = $(addsuffix .tmp, $(basename $(INTEGRATION_TESTS)))

.scm.tmp:
	@echo Running integration test $< ...
	$(INTEGRATION_RUN) $<
	$(TOUCH) $@
	@echo done.

integration-local:
	rm -f $(INTEGRATION_TEST_TARGETS)
	$(MAKE) $(AM_MAKEFLAGS) reintegration

reintegration-local: $(INTEGRATION_TEST_TARGETS)

CLEANFILES = pattern.png swap-channels.jpg *.tmp

MAINTAINERCLEANFILES = Makefile.in
