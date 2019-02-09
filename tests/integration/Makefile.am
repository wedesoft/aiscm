SUFFIXES = .scm .tmp

TESTS_ENVIRONMENT = LD_LIBRARY_PATH=$(top_builddir)/aiscm/.libs:$(LD_LIBRARY_PATH) GUILE_AUTO_COMPILE=0
SCM_LOG_COMPILE = $(GUILE) -L $(top_builddir) -L $(srcdir)
INTEGRATION_RUN = $(TESTS_ENVIRONMENT) $(SCM_LOG_COMPILE)

INTEGRATION_TESTS = 2d_array.scm 2d_matching.scm array_shape.scm byte_sequence.scm \
										camera_mode.scm grab.scm integer_type.scm pulse_in.scm pulse_out.scm spectrum.scm \
										read_image.scm roll_image.scm roll_unroll.scm swap_channels.scm typed_sequence.scm \
										type_matching.scm virtual_arrays.scm virtual_registers.scm write_image.scm \
										xorg_image_list.scm xorg_video_list.scm xorg_video.scm xorg_window.scm \
										crop_dump.scm crop_2d.scm to_list.scm complex_array.scm unary_ops.scm \
										boolean_array.scm invert.scm division.scm modulo.scm binary.scm \
										ffvideo.scm ffaudio.scm ffplay.scm pts.scm ffstream.scm ffwritevideo.scm \
										ffwriteaudio.scm ffwrite.scm callback.scm object_array.scm \
										convolution_1d.scm convolution_2d.scm roberts_cross.scm sobel.scm box_filter.scm \
										conway.scm show_gray.scm sharpen.scm xorg_scale.scm xorg_scale_video.scm \
										xorg_scale_image.scm xorg_scale_list.scm xorg_fullscreen.scm xorg_fullscreen_video.scm \
										xorg_fullscreen_test.scm anaglyph.scm indicator.scm fill.scm index.scm set.scm \
										erode.scm dilate.scm tensor.scm pseudo.scm mirror.scm xor.scm mnist.scm \
										gauss_blur.scm gauss_gradient.scm harris_stephens.scm zoom.scm \
										components.scm

EXTRA_DIST = $(INTEGRATION_TESTS) pavillion.jpg star-ferry.jpg shuttle.jpg letters.png \
						 voice-record.scm voice-train.scm voice-run.scm

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

CLEANFILES = *.tmp av-sync.mp4 fubk.png pattern.png swap-channels.jpg rolled.jpg cropped.jpg crop2d.jpg inverted.jpg \
						 divided.jpg modulo.jpg test.mp3 roberts-cross.jpg sobel.jpg box-filter.jpg sharpen.jpg eroded.jpg \
						 dilated.jpg pseudo.jpg mirror.jpg gauss-blur.jpg gauss-gradient.jpg components.png

MAINTAINERCLEANFILES = Makefile.in

mnist.tmp: train-labels-idx1-ubyte train-images-idx3-ubyte t10k-images-idx3-ubyte t10k-labels-idx1-ubyte

train-images-idx3-ubyte: train-images-idx3-ubyte.gz
	gunzip -k $<

train-labels-idx1-ubyte: train-labels-idx1-ubyte.gz
	gunzip -k $<

t10k-images-idx3-ubyte: t10k-images-idx3-ubyte.gz
	gunzip -k $<

t10k-labels-idx1-ubyte: t10k-labels-idx1-ubyte.gz
	gunzip -k $<

train-images-idx3-ubyte.gz:
	curl -o $@ http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz

train-labels-idx1-ubyte.gz:
	curl -o $@ http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz

t10k-images-idx3-ubyte.gz:
	curl -o $@ http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz

t10k-labels-idx1-ubyte.gz:
	curl -o $@ http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz
