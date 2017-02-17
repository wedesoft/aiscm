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
             (rnrs bytevectors)
             (ice-9 binary-ports)
             (system foreign)
             (aiscm mem)
             (aiscm jit)
             (aiscm element)
             (aiscm int)
             (aiscm pointer)
             (aiscm sequence)
             (aiscm rgb)
             (aiscm image))


(test-begin "aiscm image")

(load-extension "libguile-aiscm-tests" "init_tests")


(define l '((2 3 5 7) (11 13 17 19)))
(define c (list (list (rgb 2 3 5) (rgb 7 11 13)) (list (rgb 3 5 7) (rgb 5 7 11))))
(define m (to-array <ubyte> l))
(define mem (value m))
(define img (make <image> #:format 'GRAY #:shape '(8 1) #:mem mem))

(define mjpeg-bytes (call-with-input-file "fixtures/leds.mjpeg" get-bytevector-all #:binary #t))
(define mjpeg-data (make <mem> #:size (bytevector-length mjpeg-bytes)))
(write-bytes mjpeg-data mjpeg-bytes)
(define mjpeg-frame (make <image> #:format 'MJPG #:shape '(320 240) #:mem mjpeg-data))

(test-assert "Convert first element of Scheme array to integer"
  (scm-to-int-array-one-element '(123)))
(test-assert "Convert second element of Scheme array to integer"
  (scm-to-int-array-second-element '(123 456)))
(test-assert "Convert first element of Scheme array to long integer"
  (scm-to-long-array-one-element (list (ash 123 32))))
(test-assert "Convert second element of Scheme array to long integer"
  (scm-to-long-array-second-element (list (ash 123 32) (ash 456 32))))
(test-equal "conversion to BGR"
  #vu8(2 2 2 3 3 3) (read-bytes (slot-ref (convert-image img 'BGR) 'mem) 6))
(test-equal "shape of scaled image"
  '(16 2) (shape (convert-image img 'BGRA '(16 2))))
(test-assert "duplicated image should be different"
  (not (eq? img (duplicate img))))
(test-equal "values of image with scaled height"
  #vu8(2 3 5 7 11 13 17 19 2 3 5 7 11 13 17 19) (read-bytes (slot-ref (convert-image img 'GRAY '(8 2)) 'mem) 16))
(test-equal "correct application of custom pitches"
  2 (bytevector-u8-ref (read-bytes (slot-ref (convert-image img 'GRAY '(8 2) '(0) '(16)) 'mem) 32) 16))
(test-equal "'to-array' should convert the image to a 2D array"
  '((2 3 5 7 11 13 17 19)) (to-list (to-array img)))
(test-equal "'to-array' should convert the image to a colour image"
  (list (rgb 1 1 1) (rgb 2 2 2) (rgb 3 3 3)) (to-list (crop 3 (project (to-array (convert-image img 'UYVY))))))
(test-equal "'to-array' should take pitches (strides) into account"
  '(2 2) (to-list (project (roll (to-array (convert-image img 'GRAY '(8  2) '(0) '(16)))))))
(test-equal "'to-image' converts to grayscale image"
  'GRAY (get-format (to-image m)))
(test-assert "'to-image' for an image has no effect"
  (to-image img))
(test-equal "'to-image' preserves shape of array"
  '(4 2) (shape (to-image m)))
(test-equal "Converting from unsigned byte multiarray to image and back preserves data"
  l (to-list (to-array (to-image m))))
(test-equal "Conversion to image ensures compacting of pixel lines"
  #vu8(1 3 2 4) (read-bytes (slot-ref (to-image (roll (arr (1 2) (3 4)))) 'mem) 4))
(test-equal "Converting from integer multiarray to image and back converts to byte data"
  l (to-list (to-array (to-image (to-array <int> l)))))
(test-equal "Convert RGB array to image"
  c (to-list (to-array (to-image (to-array c)))))
(test-equal "Convert integer RGB array to image"
  c (to-list (to-array (to-image (to-array <intrgb> c)))))
(test-eq "Convert RGB symbol to format number and back"
  'RGB (format->symbol (symbol->format 'RGB)))
(test-eq "Convert I420 symbol to format number and back"
  'I420 (format->symbol (symbol->format 'I420)))

(test-equal "Convert MJPEG frame to RGB and test a pixel"
  (rgb 51 58 42) (get (to-array mjpeg-frame) 32 56))

(define target (to-image (arr (0 0 0 0 0 0 0 0))))
(convert-image-from! target (to-image (arr (1 2 3 4 6 7 8 9))))
(test-equal "Write conversion result to a target image"
  #vu8(1 2 3 4 6 7 8 9) (read-bytes (slot-ref target 'mem) 8))

(test-end "aiscm image")
