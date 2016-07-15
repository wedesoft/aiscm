SUFFIXES = .scm .tmp

TESTS_ENVIRONMENT = LD_LIBRARY_PATH=$(top_builddir)/aiscm/.libs:$(LD_LIBRARY_PATH) GUILE_AUTO_COMPILE=0
SCM_LOG_COMPILE = $(GUILE) -L $(top_builddir) -L $(srcdir)
INTEGRATION_RUN = $(TESTS_ENVIRONMENT) $(SCM_LOG_COMPILE)

INTEGRATION_TESTS = 2d_array.scm 2d_matching.scm array_shape.scm asm.scm byte_sequence.scm \
										camera_mode.scm grab.scm integer_type.scm jmp.scm pulse_in.scm pulse_out.scm \
										read_image.scm roll_image.scm roll_unroll.scm swap_channels.scm typed_sequence.scm \
										type_matching.scm virtual_arrays.scm virtual_registers.scm write_image.scm \
										xorg_image_list.scm xorg_video_list.scm xorg_video.scm xorg_window.scm \
										crop_dump.scm crop_2d.scm to_list.scm complex_array.scm unary_ops.scm \
										boolean_array.scm invert.scm scalars.scm division.scm modulo.scm binary.scm \
										ffvideo.scm ffaudio.scm ffplay.scm

EXTRA_DIST = $(INTEGRATION_TESTS) pavillion.jpg star-ferry.jpg

noinst_DATA = fubk.png av-sync.mp4 test.mp3

fubk.png: ../fixtures/fubk.png
	$(LN_S) $< $@

av-sync.mp4: ../fixtures/av-sync.mp4
	$(LN_S) $< $@

test.mp3: ../fixtures/test.mp3
	$(LN_S) $< $@

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

CLEANFILES = fubk.png av-sync.mp4 test.mp3 pattern.png swap-channels.jpg *.tmp

MAINTAINERCLEANFILES = Makefile.in
