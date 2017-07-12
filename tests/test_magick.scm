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
(use-modules (srfi srfi-64)
             (oop goops)
             (aiscm magick)
             (aiscm element)
             (aiscm pointer)
             (aiscm sequence)
             (aiscm jit)
             (aiscm int)
             (aiscm rgb))


(test-begin "aiscm magick")

(define ramp (read-image "fixtures/ramp.png"))
(define index (read-image "fixtures/index.png"))
(test-equal "Check size of loaded image"
  '(6 4) (shape ramp))
(test-equal "Check pixel of loaded RGB image"
  (rgb 2 1 128) (get ramp 2 1))
(test-error "Throw exception if file not found"
  'misc-error (read-image "fixtures/nonexistent.png"))
(test-equal "Check pixel of loaded greyscale image"
  18 (get index 2 1))
(define grey-file-name (string-append (tmpnam) ".png"))
(define grey-values '((1 2 3) (4 5 6)))
(define grey-img (to-array grey-values))
(define retval (write-image grey-img grey-file-name))
(test-eq "Writing image should return input image"
  retval grey-img)
(define grey (read-image grey-file-name))
(test-assert "Check content of saved greyscale image"
  (equal? grey-values (to-list grey)))
(test-error "Should handle errors when writing image"
  'misc-error (write-image grey-img "fixtures/nosuchdir/tmp.png"))
(define colour-file-name (string-append (tmpnam) ".png"))
(define colour-values
  (map (lambda (j) (map (lambda (i) (rgb i j 128)) (iota 8))) (iota 2)))
(define colour-img (to-array colour-values))
(write-image colour-img colour-file-name)
(define colour (read-image colour-file-name))
(test-equal "Check content of saved colour image"
  colour-values (to-list colour))
(test-error "Make sure image type is supported"
  'misc-error (write-image (make (multiarray <int> 2) #:shape '(6 4)) "fixtures/tmp.png"))
(test-error "Make sure image has two dimensions"
  'misc-error (write-image (make (sequence <int>) #:size 8) "fixtures/tmp.png"))
(define rolled-file-name (string-append (tmpnam) ".png"))
(write-image (roll colour-img) rolled-file-name)
(test-equal "Write image with non-default strides (pitches)"
  colour-values (to-list (roll (read-image rolled-file-name))))

(test-end "aiscm magick")
