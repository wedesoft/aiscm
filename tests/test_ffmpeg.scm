;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016, 2017 Jan Wedekind <jan@wedesoft.de>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
(use-modules (srfi srfi-1)
             (srfi srfi-64)
             (oop goops)
             (aiscm ffmpeg)
             (aiscm element)
             (aiscm int)
             (aiscm float)
             (aiscm rgb)
             (aiscm image)
             (aiscm pointer)
             (aiscm sequence))


(test-begin "aiscm ffmpeg")

(load-extension "libguile-aiscm-tests" "init_tests")

(test-begin "helper methods")
  (test-assert "Convert array with one integer to 64 bit"
    (int-array-to-long-one-element))
  (test-assert "Convert array with two integers to 64 bit"
    (int-array-to-long-second-element))
  (test-assert "Convert empty integer array to Scheme array"
    (null? (from-array-empty)))
  (test-equal "Convert integer array with three elements to Scheme array"
    '(2 3 5) (from-array-three-elements))
  (test-equal "Convert integer array to Scheme array stopping at first zero element"
    '(2 3 5) (from-array-stop-at-zero))
  (test-equal "Convert zero array with minimum number of elements"
    '(0) (from-array-at-least-one))
  (test-equal "Convert long integer array element to Scheme"
    (list (ash 42 32)) (from-array-long-integers))
  (test-assert "First value of offset-array is zero"
    (first-offset-is-zero))
  (test-assert "Second value of offset-array correct"
    (second-offset-correct))
  (test-assert "Set offset values for null pointers to zero"
    (zero-offset-for-null-pointer))
  (test-assert "Offsets have 64 bit"
    (offsets-have-64-bit))
  (test-assert "Pack byte audio sample"
    (pack-byte-audio-sample))
  (test-assert "Pack byte audio samples"
    (pack-byte-audio-samples))
  (test-assert "Pack short integer audio samples"
    (pack-short-int-audio-samples))

  (define-class <dummy> ()
    (buffer #:init-value '())
    (clock  #:init-value 0))
  (let [(dummy (make <dummy>))]
    (test-assert "Popping buffer should return #f when empty"
      (not (ffmpeg-buffer-pop dummy 'buffer 'clock))))
  (let [(dummy (make <dummy>))]
    (ffmpeg-buffer-push dummy 'buffer (cons 123 'dummy-frame))
    (ffmpeg-buffer-push dummy 'buffer (cons 456 'other-frame))
    (test-eq "Popping buffer should return first frame"
      'dummy-frame (ffmpeg-buffer-pop dummy 'buffer 'clock))
    (test-eq "Popping buffer should set the time stamp"
      123 (slot-ref dummy 'clock))
    (test-eq "Popping buffer again should return the second frame"
      'other-frame (ffmpeg-buffer-pop dummy 'buffer 'clock))
    (test-eq "Popping buffer again should set the time stamp"
      456 (slot-ref dummy 'clock)))
(test-end "helper methods")

(test-begin "video input")
  (define input-video (open-ffmpeg-input "fixtures/av-sync.mp4"))

  (test-assert "'open-ffmpeg-input' creates an FFmpeg object"
    (is-a? input-video <ffmpeg>))
  (test-equal "Check frame size of input video"
    '(640 360) (shape input-video))
  (test-eqv "Get frame rate of input video"
    25 (frame-rate input-video))
  (test-equal "Get aspect ratio of input video"
    1 (aspect-ratio input-video))
  (test-eqv "Detect stereo audio stream"
    2 (channels input-video))

  (define video-pts0 (video-pts input-video))
  (define video-frame (read-image input-video))

  (test-assert "Check that video frame is an image object"
    (is-a? video-frame <image>))
  (test-equal "Check shape of video frame"
    '(640 360) (shape video-frame))
  (test-equal "Check a pixel in the first video frame of the video"
    (rgb 154 154 154) (get (to-array video-frame) 10 270))

  (define video-pts1 (video-pts input-video))
  (read-image input-video)
  (define video-pts2 (video-pts input-video))

  (test-equal "Check first three video frame time stamps"
    (list 0 0 (/ 1 25)) (list video-pts0 video-pts1 video-pts2))

  (pts= input-video 15)
  (read-image input-video)

  (test-assert "Seeking audio/video should update the video position"
    (<= 15 (video-pts input-video)))

  (define full-video (open-ffmpeg-input "fixtures/av-sync.mp4"))
  (define images (map (lambda _ (read-image full-video)) (iota 2253)))

  (test-assert "Check last image of video was read"
    (last images))
  (test-assert "Check 'read-image' returns false after last frame"
    (not (read-image full-video)))

  (test-error "Throw exception when trying to write to input video"
    'misc-error (write-image colour-image (open-ffmpeg-input "fixtures/av-sync.mp4")))
  (test-error "Throw error if file does not exist"
    'misc-error (open-ffmpeg-input "fixtures/no-such-file.avi"))
(test-end "video input")

(test-begin "image input")
  (define image (open-ffmpeg-input "fixtures/fubk.png"))

  (test-assert "Image has only one video frame"
    (not (cadr (list (read-image image) (read-image image)))))
  (test-assert "Do not hang when reading audio from image"
    (not (read-audio image))); test should not hang

  (test-error "Image does not have audio channels"
    'misc-error (channels image))
  (test-error "Image does not have an audio sampling rate"
    'misc-error (rate image))
  (test-error "Image does not have an audio sample type"
    'misc-error (typecode image))

  (define image (open-ffmpeg-input "fixtures/fubk.png"))
  (read-audio image)
  (test-assert "Cache video data when reading audio"
    (read-image image))
(test-end "image input")

(test-begin "video output")
  (define colour-values
    (map (lambda (j) (map (lambda (i) (rgb i j 128)) (iota 8))) (iota 2)))
  (define colour-array (to-array colour-values))
  (define colour-image (to-image colour-array))

  (define output-file (string-append (tmpnam) ".avi"))
  (define output-video (open-ffmpeg-output output-file
                                           #:format-name 'avi
                                           #:shape '(16 2)
                                           #:frame-rate 25
                                           #:video-bit-rate 100000
                                           #:aspect-ratio (/ 16 11)))

  (test-assert "'open-ffmpeg-output' creates an FFmpeg object"
    (is-a? output-video <ffmpeg>))
  (test-equal "Check frame size of output video"
    '(16 2) (shape output-video))
  (test-eqv "Get video bit-rate of output video"
    100000 (video-bit-rate output-video))
  (test-equal "Get aspect ratio of output video"
    (/ 16 11) (aspect-ratio output-video))
  (test-equal "Get frame rate of output video"
    25 (frame-rate output-video))

  (test-eq "Writing a 2D array with 'write-image' returns the input array"
    colour-array (write-image colour-array output-video))
  (test-eq "Writing an image with 'write-image' returns the input image"
    colour-image (write-image colour-image output-video))

  (test-error "Throw exception if output file cannot be written"
    'misc-error (open-ffmpeg-output "no-such-folder/test.mpg"))
  (test-error "Throw exception if output format is not known"
    'misc-error (open-ffmpeg-output (string-append (tmpnam) ".avi") #:format-name 'nosuchformat))
  (test-error "Throw exception when trying to read from output video"
    'misc-error (read-image (open-ffmpeg-output (string-append (tmpnam) ".avi"))))
  (test-error "Throw exception when trying to seek in output video"
    'misc-error (pts= (open-ffmpeg-output (string-append (tmpnam) ".avi")) 10))
(test-end "video output")

(test-begin "audio input")
  (define audio-mono (open-ffmpeg-input "fixtures/mono.mp3"))

  (test-error "Audio file does not have width and height"
    'misc-error (shape audio-mono))
  (test-error "Audio file does not have a frame rate"
    'misc-error (frame-rate audio-mono))
  (test-eqv "Detect mono audio stream"
    1 (channels audio-mono))
  (test-eqv "Get sampling rate of audio stream"
    8000 (rate audio-mono))
  (test-eq "Get type of audio samples"
    <sint> (typecode audio-mono))

  (define audio-pts0 (audio-pts audio-mono))
  (define audio-mono-frame (read-audio audio-mono))

  (test-assert "Check that audio frame is an array"
    (is-a? audio-mono-frame <sequence<>>))
  (test-eqv "Get a value from a mono audio frame"
    40 (get audio-mono-frame 0 300))
  (test-eqv "Audio frame should have two dimensions"
    2 (dimensions audio-mono-frame))
  (test-eqv "Mono audio frame should have 1 as first dimension"
    1 (car (shape audio-mono-frame)))
  (test-eq "Audio frame should have samples of correct type"
    <sint> (typecode audio-mono-frame))

  (define audio-pts1 (audio-pts audio-mono))
  (read-audio audio-mono)
  (define audio-pts2 (audio-pts audio-mono))

  (test-equal "Check first three audio frame time stamps"
    (list 0 0 (/ 3456 48000)) (list audio-pts0 audio-pts1 audio-pts2))

  (define audio-stereo (open-ffmpeg-input "fixtures/test.mp3"))
  (define audio-stereo-frame (read-audio audio-stereo))

  (test-eqv "Stereo audio frame should have 2 as first dimension"
    2 (car (shape audio-stereo-frame)))

  (define full-audio (open-ffmpeg-input "fixtures/test.mp3"))
  (define samples (map (lambda _ (read-audio full-audio)) (iota 1625)))

  (test-skip 1)
  (test-assert "Check 'read-audio' returns false after last frame"
    (not (read-audio full-audio))); number of audio frames depends on FFmpeg version
(test-end "audio input")

(test-end "aiscm ffmpeg")
