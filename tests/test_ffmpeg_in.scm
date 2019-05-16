;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018, 2019 Jan Wedekind <jan@wedesoft.de>
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
             (aiscm samples)
             (aiscm image)
             (aiscm core))


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

  (test-begin "convert pointers to offsets")
    (test-assert "First value of offset-array is zero"
      (first-offset-is-zero))
    (test-assert "Second value of offset-array correct"
      (second-offset-correct))
    (test-assert "Set offset values for null pointers to zero"
      (zero-offset-for-null-pointer))
    (test-assert "Offsets have 64 bit"
      (offsets-have-64-bit (ash 1 62)))
  (test-end "convert pointers to offsets")

  (test-assert "Pack byte audio sample"
    (pack-byte-audio-sample))
  (test-assert "Pack byte audio samples"
    (pack-byte-audio-samples))
  (test-assert "Pack short integer audio samples"
    (pack-short-int-audio-samples))

  (define-class <dummy> ()
    (video-buffer #:init-value '())
    (video-pts  #:init-value 0))
  (let [(dummy (make <dummy>))]
    (test-assert "Popping buffer should return #f when empty"
      (not (video-buffer-pop dummy))))
  (let [(dummy (make <dummy>))]
    (video-buffer-push dummy (cons 123 'dummy-frame))
    (video-buffer-push dummy (cons 456 'other-frame))
    (test-eq "Popping buffer should return first frame"
      'dummy-frame (video-buffer-pop dummy))
    (test-eq "Popping buffer should set the time stamp"
      123 (slot-ref dummy 'video-pts))
    (test-eq "Popping buffer again should return the second frame"
      'other-frame (video-buffer-pop dummy))
    (test-eq "Popping buffer again should set the time stamp"
      456 (slot-ref dummy 'video-pts)))
(test-end "helper methods")

(test-begin "video input")
  (define input-video (open-ffmpeg-input "fixtures/av-sync.mp4"))

  (test-assert "Video input is an input object"
    (is-input? input-video))
  (test-assert "Video has video stream"
    (have-video? input-video))
  (test-assert "'open-ffmpeg-input' creates an FFmpeg object"
    (is-a? input-video <ffmpeg>))
  (test-equal "Check frame size of input video"
    '(360 640) (shape input-video))
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
    '(360 640) (shape video-frame))
  (test-equal "Check a pixel in the first video frame of the video"
    (rgb 154 154 154) (get (from-image video-frame) 10 270))

  (define video-pts1 (video-pts input-video))
  (read-image input-video)
  (define video-pts2 (video-pts input-video))

  (test-equal "Check first three video frame time stamps"
    (list 0 0 (/ 1 25)) (list video-pts0 video-pts1 video-pts2))

  (pts= input-video 15)
  (read-image input-video)

  (test-assert "Seeking audio/video should update the video position"
    (<= 15 (video-pts input-video)))

  (buffer-timestamped-video 15 input-video)
  (pts= input-video 15)
  (test-assert "Flush video buffer when seeking in input video"
    (zero? (video-buffer-fill input-video)))

  (destroy input-video)

  (define full-video (open-ffmpeg-input "fixtures/av-sync.mp4"))
  (define images (map (lambda _ (read-image full-video)) (iota 2253)))
  (test-assert "Check last image of video was read"
    (last images))
  (test-assert "Check 'read-image' returns false after last frame"
    (not (read-image full-video)))
  (destroy full-video)

  (test-error "Throw exception when trying to write to input video"
    'misc-error (write-image colour-image (open-ffmpeg-input "fixtures/av-sync.mp4")))
  (test-error "Throw error if file does not exist"
    'misc-error (open-ffmpeg-input "fixtures/no-such-file.avi"))
(test-end "video input")

(test-begin "image input")
  (define decoded (decode-audio/video (open-ffmpeg-input "fixtures/fubk.png")))

  (test-eq "Decoding image data results in a video frame"
    'video (car decoded))
  (test-eqv "Decoding image data results a trivial time stamp"
    0 (cadr decoded))

  (define image (open-ffmpeg-input "fixtures/fubk.png"))
  (decode-audio/video image)
  (test-assert "Return false after decoding last frame"
    (not (decode-audio/video image)))

  (define image (open-ffmpeg-input "fixtures/fubk.png"))
  (test-eqv "Video buffer is empty initially"
    0 (video-buffer-fill image))
  (decode-audio/video image)
  (test-assert "Buffering video should return true"
    (buffer-timestamped-video 123 image))
  (test-eqv "Video buffer contains one frame after buffering a frame"
    1 (video-buffer-fill image))
  (test-equal "Timestamp is stored in buffer"
    123 (caar (slot-ref image 'video-buffer)))
  (test-equal "Shape of buffered video frame is the same"
    '(288 384) (shape (cdar (slot-ref image 'video-buffer))))
  (test-assert "Stored frame is a duplicate (i.e. not the same)"
    (not (eq? video-frame (cdar (slot-ref image 'video-buffer)))))

  (define image (open-ffmpeg-input "fixtures/fubk.png"))
  (test-assert "Image has only one video frame"
    (not (cadr (list (read-image image) (read-image image)))))
  (test-assert "Image does not have audio data"
    (not (have-audio? image)))
  (test-assert "Do not hang when reading audio from image"
    (not (read-audio image 4410))); test should not hang

  (test-error "Image does not have audio channels"
    'misc-error (channels image))
  (test-error "Image does not have an audio sampling rate"
    'misc-error (rate image))
  (test-error "Image does not have an audio sample type"
    'misc-error (typecode image))
  (destroy image)

  (define image (open-ffmpeg-input "fixtures/fubk.png"))
  (read-audio image 4410)
  (test-assert "Cache video data when reading audio"
    (read-image image))
  (destroy image)
(test-end "image input")

(test-begin "audio input")
  (define decoded (decode-audio/video (open-ffmpeg-input "fixtures/mono.mp3")))
  (test-eq "Decoding audio data results in an audio frame"
    'audio (car decoded))
  (test-eqv "Decoding audio data results a trivial time stamp"
    0 (cadr decoded))

  (define audio-mono (open-ffmpeg-input "fixtures/mono.mp3"))
  (test-assert "Audio file does not have a video stream"
    (not (have-video? audio-mono)))
  (test-assert "Do not hang when attempting to read an image from an audio file"
    (not (read-image audio-mono)))
  (test-assert "Audio input has audio stream"
    (have-audio? audio-mono))
  (test-eqv "Audio buffer fill is zero initially"
    0 (audio-buffer-fill audio-mono))
  (define audio-samples (read-audio audio-mono 4410))
  (test-equal "Retrieve specified number of audio samples"
    '(4410 1) (shape audio-samples))
  (test-eq "Typecode of samples is typecode of input"
    (typecode audio-mono) (typecode audio-samples))
  (test-eq "Rate of samples is rate of input"
    (rate audio-mono) (rate audio-samples))
  (test-assert "Samples are packed"
    (not (planar? audio-samples)))
(test-end "audio input")

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
  (test-skip 1)
  (test-eq "Get type of audio samples"
    <sint> (typecode audio-mono))

  (define audio-mono (open-ffmpeg-input "fixtures/mono.mp3"))
  (decode-audio/video audio-mono)
  (test-assert "Buffering audio should return true"
    (buffer-timestamped-audio 123 audio-mono))
  (test-assert "Buffering input audio should increase the buffer size"
    (not (zero? (audio-buffer-fill audio-mono))))

  (define wav (open-ffmpeg-input "fixtures/cat.wav"))
  (test-assert "Query typecode of WAV file"
    (typecode wav))
  (test-assert "Read from WAV file"
    (read-audio wav 4410))

  (define samples (to-samples (to-array <sint> '((2) (3) (5) (7) (3) (4) (6) (8))) 8000))
  (fetch-audio audio-mono samples)
  (test-skip 1)
  (test-equal "Fetching back buffered audio should maintain the values"
    '((0) (0) (0) (0) (0) (0) (0) (0)) (to-list (to-array samples)))

  (define audio-mono (open-ffmpeg-input "fixtures/mono.mp3"))
  (define audio-pts0 (audio-pts audio-mono))
  (define audio-mono-frame (read-audio audio-mono 4410))

  (test-assert "Check that audio frame is a set of samples"
    (is-a? audio-mono-frame <samples>))
  (test-skip 1)
  (test-eqv "Get a value from a mono audio frame"
    40 (get (to-array audio-mono-frame) 0 300))
  (test-eqv "Mono audio frame should have 1 channel"
    1 (channels audio-mono-frame))
  (test-equal "Mono audio frame should have the desired shape"
    '(4410 1) (shape audio-mono-frame))
  (test-skip 1)
  (test-eq "Audio frame should have samples of correct type"
    <sint> (typecode audio-mono-frame))

  (define audio-mono (open-ffmpeg-input "fixtures/mono.mp3"))
  (define audio-pts0 (audio-pts audio-mono))
  (read-audio audio-mono 800)
  (define audio-pts1 (audio-pts audio-mono))
  (read-audio audio-mono 800)
  (define audio-pts2 (audio-pts audio-mono))
  (test-equal "Check first three audio frame time stamps"
    (list 0 (/ 1 10) (/ 2 10)) (list audio-pts0 audio-pts1 audio-pts2))

  (pts= audio-mono 1)
  (test-eqv "Seeking in the audio stream should flush the audio buffer"
    0 (audio-buffer-fill audio-mono))

  (pts= audio-mono 35)
  (test-assert "Return a smaller frame when attempting to read beyond the end of the audio stream"
    (< (cadr (shape (read-audio audio-mono 441000))) 441000)); test should not hang
  (test-assert "Check 'read-audio' returns false after last audio sample"
    (not (read-audio audio-mono 4410)))

  (destroy audio-mono)

  (define audio-stereo (open-ffmpeg-input "fixtures/test.mp3"))

  (test-eqv "Stereo audio frame should have 2 channels"
    2 (channels (read-audio audio-stereo 4410)))

  (destroy audio-stereo)
(test-end "audio input")

(test-end "aiscm ffmpeg")
