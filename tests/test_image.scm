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
(use-modules (oop goops)
             (rnrs bytevectors)
             (ice-9 binary-ports)
             (system foreign)
             (srfi srfi-1)
             (aiscm mem)
             (aiscm jit)
             (aiscm element)
             (aiscm int)
             (aiscm pointer)
             (aiscm sequence)
             (aiscm rgb)
             (aiscm image)
             (guile-tap))
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

(ok (scm-to-int-array-one-element '(123))
    "Convert first element of Scheme array to integer")
(ok (scm-to-int-array-second-element '(123 456))
    "Convert second element of Scheme array to integer")
(ok (scm-to-long-array-one-element (list (ash 123 32)))
    "Convert first element of Scheme array to long integer")
(ok (scm-to-long-array-second-element (list (ash 123 32) (ash 456 32)))
    "Convert second element of Scheme array to long integer")
(diagnostics "following test only works with recent version of libswscale")
(ok (equal? #vu8(2 2 2 3 3 3) (read-bytes (get-mem (convert img 'BGR)) 6))
  "conversion to BGR")
(ok (equal? '(16 2) (shape (convert img 'BGRA '(16 2))))
  "shape of scaled image")
(ok (not (eq? img (duplicate img)))
  "duplicated image should be different")
(ok (equal? #vu8(2 3 5 7 11 13 17 19 2 3 5 7 11 13 17 19)
            (read-bytes (get-mem (convert img 'GRAY '(8 2))) 16))
  "values of image with scaled height")
(ok (equal? 2 (bytevector-u8-ref (read-bytes (get-mem (convert img 'GRAY '(8 2) '(0) '(16))) 32) 16))
  "correct application of custom pitches")
(ok (equal? '((2 3 5 7 11 13 17 19)) (to-list (to-array img)))
  "'to-array' should convert the image to a 2D array")
(diagnostics "following test only works with recent version of libswscale")
(ok (equal? (list (rgb 1 1 1) (rgb 2 2 2) (rgb 3 3 3)) (to-list (crop 3 (project (to-array (convert img 'UYVY))))))
  "'to-array' should convert the image to a colour image")
(ok (equal? '(2 2) (to-list (project (roll (to-array (convert img 'GRAY '(8  2) '(0) '(16)))))))
  "'to-array' should take pitches (strides) into account")
(ok (equal? 'GRAY (get-format (to-image m)))
  "'to-image' converts to grayscale image")
(ok (to-image img)
  "'to-image' for an image has no effect")
(ok (equal? '(4 2) (shape (to-image m)))
  "'to-image' preserves shape of array")
(ok (equal? l (to-list (to-array (to-image m))))
  "Converting from unsigned byte multiarray to image and back preserves data")
(ok (equal? #vu8(1 3 2 4) (read-bytes (get-mem (to-image (roll (arr (1 2) (3 4))))) 4))
  "Conversion to image ensures compacting of pixel lines")
(ok (equal? l (to-list (to-array (to-image (to-array <int> l)))))
  "Converting from integer multiarray to image and back converts to byte data")
(ok (equal? c (to-list (to-array (to-image (to-array c)))))
  "Convert RGB array to image")
(ok (equal? c (to-list (to-array (to-image (to-array <intrgb> c)))))
  "Convert integer RGB array to image")
(ok (eq? 'RGB (format->symbol (symbol->format 'RGB)))
  "Convert RGB symbol to format number and back")
(ok (eq? 'I420 (format->symbol (symbol->format 'I420)))
  "Convert I420 symbol to format number and back")

(ok (equal? (rgb 51 58 42) (get (to-array mjpeg-frame) 32 56))
    "Convert MJPEG frame to RGB and test a pixel")

(define target (to-image (arr (0 0 0 0 0 0 0 0))))
(convert-from! target (to-image (arr (1 2 3 4 6 7 8 9))))
(ok (equal? #vu8(1 2 3 4 6 7 8 9) (read-bytes (get-mem target) 8))
    "Write conversion result to a target image")

(run-tests)
