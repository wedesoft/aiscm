SUFFIXES = .scm .tmp

TESTS_ENVIRONMENT = LD_LIBRARY_PATH=$(top_builddir)/aiscm/.libs:$(LD_LIBRARY_PATH)
SCM_LOG_COMPILE = $(GUILE) --no-auto-compile -L $(top_builddir) -L $(srcdir)
INTEGRATION_RUN = $(TESTS_ENVIRONMENT) $(SCM_LOG_COMPILE)

INTEGRATION_TESTS = 2d_array.scm 2d_matching.scm array_shape.scm asm.scm byte_sequence.scm \
										camera_mode.scm grab.scm integer_type.scm jmp.scm pulse_in.scm pulse_out.scm \
										read_image.scm roll_image.scm roll_unroll.scm swap_channels.scm typed_sequence.scm \
										type_matching.scm virtual_arrays.scm virtual_registers.scm write_image.scm \
										xorg_image_list.scm xorg_video_list.scm xorg_video.scm xorg_window.scm \
										crop_dump.scm crop_2d.scm to_list.scm complex_array.scm unary_ops.scm \
										boolean_array.scm invert.scm scalars.scm

EXTRA_DIST = $(INTEGRATION_TESTS) fubk.png pavillion.jpg star-ferry.jpg 

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
