;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018 Jan Wedekind <jan@wedesoft.de>
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
             (aiscm core)
             (aiscm util)
             (aiscm image))


(test-begin "aiscm image")

(define l '((2 3 5 7) (11 13 17 19)))
(define c (list (list (rgb 2 3 5) (rgb 7 11 13)) (list (rgb 3 5 7) (rgb 5 7 11))))
(define m (to-array l))
(define mem (memory m))
(define img (make <image> #:format 'GRAY #:shape '(8 1) #:memory mem))

(define mjpeg-bytes (call-with-input-file "fixtures/leds.mjpeg" get-bytevector-all #:binary #t))
(define mjpeg-frame (make <image> #:format 'MJPG #:shape '(320 240) #:memory (bytevector->pointer mjpeg-bytes)))

(test-equal "conversion to BGR"
  #vu8(2 2 2 3 3 3) (pointer->bytevector (memory (convert-image img 'BGR)) 6))
(test-equal "shape of scaled image"
  '(16 2) (shape (convert-image img 'BGRA '(16 2))))
(test-assert "duplicated image should be different"
  (not (eq? img (duplicate img))))
(test-equal "values of image with scaled height"
  #vu8(2 3 5 7 11 13 17 19 2 3 5 7 11 13 17 19) (pointer->bytevector (memory (convert-image img 'GRAY '(8 2))) 16))
(test-equal "correct application of custom pitches"
  2 (bytevector-u8-ref (pointer->bytevector (memory (convert-image img 'GRAY '(8 2) '(0) '(16))) 32) 16))
(test-equal "'to-array' should convert the image to a 2D array"
  '((2 3 5 7 11 13 17 19)) (to-list (to-array img)))
(test-equal "'to-array' should convert the image to a colour image"
  (rgb 3 3 3) (get (to-array (convert-image img 'UYVY)) 2 0))
(test-equal "'to-array' should take pitches (strides) into account"
  2 (get (to-array (convert-image img 'GRAY '(8  2) '(0) '(16))) 0 1))
(test-equal "'to-image' converts to grayscale image"
  'GRAY (get-format (to-image m)))
(test-assert "'to-image' for an image has no effect"
  (to-image img))
(test-equal "'to-image' preserves shape of array"
  '(4 2) (shape (to-image m)))
(test-equal "Converting from unsigned byte multiarray to image and back preserves data"
  l (to-list (to-array (to-image m))))
(test-equal "Conversion to image ensures compacting of pixel lines"
  #vu8(1 3 2 4) (pointer->bytevector (memory (to-image (roll (to-array '((1 2) (3 4)))))) 4))
(test-equal "Converting from integer multiarray to image and back converts to byte data"
  l (to-list (to-array (to-image (to-array l)))))
(test-equal "Convert RGB array to image"
  c (to-list (to-array (to-image (to-array c)))))
(test-skip 1)
(test-equal "Convert integer RGB array to image"
  c (to-list (to-array (to-image (to-array <intrgb> c)))))
(test-eq "Convert RGB symbol to format number and back"
  'RGB (format->symbol (symbol->format 'RGB)))
(test-eq "Convert I420 symbol to format number and back"
  'I420 (format->symbol (symbol->format 'I420)))

(test-equal "Convert MJPEG frame to RGB and test a pixel"
  (rgb 51 58 42) (get (to-array mjpeg-frame) 32 56))

(define target (to-image (to-array '((0 0 0 0 0 0 0 0)))))
(convert-image-from! target (to-image (to-array '((1 2 3 4 6 7 8 9)))))
(test-equal "Write conversion result to a target image"
  #vu8(1 2 3 4 6 7 8 9) (pointer->bytevector (memory target) 8))

(test-end "aiscm image")
