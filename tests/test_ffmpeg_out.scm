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
             (aiscm samples)
             (aiscm pointer)
             (aiscm sequence))


(test-begin "aiscm ffmpeg")

(test-begin "video output")
  (define colour-values
    (map (lambda (j) (map (lambda (i) (rgb i j 128)) (iota 8))) (iota 2)))
  (define colour-array (to-array colour-values))
  (define colour-image (to-image colour-array))

  (test-assert "'open-ffmpeg-output' creates an output video by default"
    (frame-rate (open-ffmpeg-output (string-append (tmpnam) ".avi"))))

  (define output-video (open-ffmpeg-output (string-append (tmpnam) ".avi")
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

  (test-equal "Get shape of target video frame"
    '(16 2) (shape (target-video-frame output-video)))
  (test-eq "Get format of target video frame"
    'I420 (get-format (target-video-frame output-video)))

  (test-eq "Writing a 2D array with 'write-image' returns the input array"
    colour-array (write-image colour-array output-video))
  (test-eq "Writing an image with 'write-image' returns the input image"
    colour-image (write-image colour-image output-video))
  (destroy output-video)

  (test-error "Throw exception when trying to read from output video"
    'misc-error (read-image (open-ffmpeg-output (string-append (tmpnam) ".avi"))))
  (test-error "Throw exception when trying to seek in output video"
    'misc-error (pts= (open-ffmpeg-output (string-append (tmpnam) ".avi")) 10))
  (test-error "Throw exception if output file cannot be written"
    'misc-error (open-ffmpeg-output "no-such-folder/test.mpg"))
  (test-error "Throw exception if output format is not known"
    'misc-error (open-ffmpeg-output (string-append (tmpnam) ".avi") #:format-name 'nosuchformat))
  (test-error "Throw exception when trying to write video to audio file"
    'misc-error (open-ffmpeg-output (string-append (tmpnam) ".mp3") #:shape '(384 288) #:frame-rate 10))
  ; TODO: test error when attempting to write video to MP3 file
(test-end "video output")

(test-begin "audio output")
  (test-assert "'open-ffmpeg-output' can create an audio file"
    (open-ffmpeg-output (string-append (tmpnam) ".mp3") #:format-name 'mp3 #:rate 44100))

  (define output-audio (open-ffmpeg-output (string-append (tmpnam) ".mp3")
                                           #:format-name 'mp3
                                           #:rate 44100
                                           #:typecode <sint>
                                           #:channels 2
                                           #:audio-bit-rate 64000))

  (test-eqv "Get number of audio channels"
    2 (channels output-audio))
  (test-eqv "Get sample rate"
    44100 (rate output-audio))
  (test-eq "Get audio sample type"
    <sint> (typecode output-audio))

  (define samples (make (multiarray <sint> 2) #:shape '(2 44100)))
  (test-equal "Writing audio samples returns samples"
    samples (write-audio samples output-audio))

  (test-begin "select-rate")
    (test-eqv "Return specified rate if supported"
      44100 ((select-rate 44100) '(44100)))
    (test-error "Throw exception if sample rate is not supported"
      'misc-error ((select-rate 44100) '(22050)))
    (test-error "Throw exception if sample rate is not supported"
      'misc-error ((select-rate 44100) '(22050)))
    (test-eqv "Use function if supplied"
      22050 ((select-rate last) '(44100 22050)))
    (test-eqv "Return desired rate if empty list is supplied"
      8000 ((select-rate 8000) '()))
  (test-end "select-rate")

  (define negotiate-audio-rate (open-ffmpeg-output (string-append (tmpnam) ".mp3")
                                                   #:format-name 'mp3
                                                   #:rate (lambda (rates) 22050)))
  (test-eqv "Set audio rate using a method"
    22050 (rate negotiate-audio-rate))

  (test-begin "select-sample-typecode")
    (test-eq "Select short integer sample format if it is supported"
      <sint> (select-sample-typecode <sint> (list <sint>)))
    (test-eq "Select larger type if desired type is not supported"
      <int> (select-sample-typecode <sint> (list <int>)))
    (test-eq "Select short integer sample format if it is the last supported one"
      <sint> (select-sample-typecode <sint> (list <int> <sint>)))
    (test-eq "Select short integer sample format if it is the first supported one"
      <sint> (select-sample-typecode <sint> (list <sint> <int>)))
    (test-eq "Select largest integer format if the desired format is not supported"
      <int> (select-sample-typecode <sint> (list <ubyte> <int>)))
    (test-eq "Select largest integer format if the desired format is not supported (regardless of order)"
      <int> (select-sample-typecode <sint> (list <int> <ubyte>)))
    (test-error "Throw exception if all supported types are smaller"
      'misc-error (select-sample-typecode <sint> (list <ubyte>)))
    (test-eq "Prefer integer format if desired format is integer"
      <int> (select-sample-typecode <int> (list <float> <int>)))
    (test-eq "Switch to floating point if required"
      <float> (select-sample-typecode <int> (list <sint> <float>)))
    (test-eq "Select specified sample format if no supported format is specified"
      <float> (select-sample-typecode <float> '()))
  (test-end "select-sample-typecode")

  (test-begin "typecodes-of-sample-formats")
    (test-equal "No supported sample types means no supported typecodes"
      '() (typecodes-of-sample-formats '()))
    (test-equal "Get the typecode of a packed sample format"
      (list <ubyte>) (typecodes-of-sample-formats (list AV_SAMPLE_FMT_U8)))
    (test-equal "Omit duplicate typecodes"
      (list <float>) (typecodes-of-sample-formats (list AV_SAMPLE_FMT_FLT AV_SAMPLE_FMT_FLTP)))
  (test-end "typecodes-of-sample-formats")

  (test-begin "best-sample-format")
    (test-eqv "Select format if it the typecode is matching"
      AV_SAMPLE_FMT_S32 (best-sample-format <int> (list AV_SAMPLE_FMT_S32)))
    (test-eqv "Ignore formats with different typecode"
      AV_SAMPLE_FMT_S32 (best-sample-format <int> (list AV_SAMPLE_FMT_S16 AV_SAMPLE_FMT_S32)))
    (test-eqv "Ignore formats with different typecode regardless of order"
      AV_SAMPLE_FMT_S32 (best-sample-format <int> (list AV_SAMPLE_FMT_S32 AV_SAMPLE_FMT_S16)))
    (test-eqv "Select planar format if packed format is not supported"
      AV_SAMPLE_FMT_S32P (best-sample-format <int> (list AV_SAMPLE_FMT_S32P)))
    (test-eqv "Select planar format if packed format is not supported (short integer case)"
      AV_SAMPLE_FMT_S16P (best-sample-format <sint> (list AV_SAMPLE_FMT_S16P)))
    (test-eqv "Prefer packed format"
      AV_SAMPLE_FMT_S32 (best-sample-format <int> (list AV_SAMPLE_FMT_S32P AV_SAMPLE_FMT_S32)))
    (test-eqv "Specify planar format if list of supported formats is empty"
      AV_SAMPLE_FMT_S32P (best-sample-format <int> '()))
  (test-end "best-sample-format")

  (test-begin "select-sample-format")
    (test-eqv "Select packed format if supported"
      AV_SAMPLE_FMT_S32 ((select-sample-format <int>) (list AV_SAMPLE_FMT_S32)))
    (test-eqv "Select nearest larger type if necessary"
      AV_SAMPLE_FMT_FLT ((select-sample-format <int>) (list AV_SAMPLE_FMT_S16 AV_SAMPLE_FMT_FLT)))
    (test-eqv "Prefer packed format"
      AV_SAMPLE_FMT_S32 ((select-sample-format <int>) (list AV_SAMPLE_FMT_S32 AV_SAMPLE_FMT_S32P)))
  (test-end "select-sample-format")

  (test-eq "Get type of target audio frame"
    <sint> (typecode (target-audio-frame output-audio)))
  (test-eqv "Get channels of target audio frame"
    2 (channels (target-audio-frame output-audio)))
  (test-eqv "Get rate of target audio frame"
    44100 (rate (target-audio-frame output-audio)))
(test-end "audio output")

(test-end "aiscm ffmpeg")
