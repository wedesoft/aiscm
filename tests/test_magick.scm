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
(use-modules (aiscm magick)
             (aiscm element)
             (aiscm pointer)
             (aiscm sequence)
             (aiscm jit)
             (aiscm rgb)
             (guile-tap))
(define ramp (read-image "fixtures/ramp.png"))
(define index (read-image "fixtures/index.png"))
(ok (equal? '(6 4) (shape ramp))
    "Check size of loaded image")
(ok (equal? (rgb 2 1 128) (get ramp 2 1))
    "Check pixel of loaded RGB image")
(ok (throws? (read-image "fixtures/nonexistent.png"))
    "Throw exception if file not found")
(ok (equal? 18 (get index 2 1))
    "Check pixel of loaded greyscale image")
(define grey-file-name (string-append (tmpnam) ".png"))
(define grey-values '((1 2 3) (4 5 6)))
(define grey-img (to-array grey-values))
(define retval (write-image grey-img grey-file-name))
(ok (eq? retval grey-img)
    "Writing image should return input image")
(define grey (read-image grey-file-name))
(diagnostics "following test works with ImageMagick 6.8.9")
(skip (equal? grey-values (to-list grey))
    "Check content of saved greyscale image")
(ok (throws? (write-image grey-img "fixtures/nosuchdir/tmp.png"))
    "Should handle errors when writing image")
(define colour-file-name (string-append (tmpnam) ".png"))
(define colour-values
  (map (lambda (j) (map (lambda (i) (rgb i j 128)) (iota 8))) (iota 2)))
(define colour-img (to-array colour-values))
(write-image colour-img colour-file-name)
(define colour (read-image colour-file-name))
(ok (equal? colour-values (to-list colour))
    "Check content of saved colour image")
(ok (throws? (write-image (make (multiarray <int> 2) #:shape '(6 4)) "fixtures/tmp.png"))
    "Make sure image type is supported")
(ok (throws? (write-image (make (sequence <int>) #:size 8) "fixtures/tmp.png"))
    "Make sure image has two dimensions")
(define rolled-file-name (string-append (tmpnam) ".png"))
(write-image (roll colour-img) rolled-file-name)
(ok (equal? colour-values (to-list (roll (read-image rolled-file-name))))
    "Write image with non-default strides (pitches)")
(run-tests)
