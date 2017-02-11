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

(test-skip 1)
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
    (open-ffmpeg-output (string-append (tmpnam) ".mp3") #:format-name 'mp3 #:sample-rate 44100))
  ; TODO: more parameters
  ; TODO: test writing audio packets
(test-end "audio output")

(test-end "aiscm ffmpeg")
