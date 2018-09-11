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
(define-module (aiscm magick)
  #:use-module (oop goops)
  #:use-module (aiscm util)
  #:use-module (aiscm core))

(load-extension "libguile-aiscm-magick" "init_magick")
(define-method (read-image (file-name <string>))
  (let [(picture    (magick-read-image file-name))
        (array-type (lambda (format) (multiarray (if (eq? format 'I) <ubyte> <rgb<ubyte>>) 2)))
        (array      (lambda (array-type shape base mem)
          (make array-type #:shape shape #:memory-base base #:memory mem)))]
    (apply (lambda (format shape base mem size)
             (array (array-type format) shape base mem))
           picture)))
(define-method (write-image (img <multiarray<>>) (file-name <string>))
  (let [(format    (cond ((eq? (typecode img) <ubyte>     ) 'I  )
                         ((eq? (typecode img) <rgb<ubyte>>) 'RGB)
                         (else #f)))
        (compacted (ensure-default-strides img))]
    (if (not format)
      (aiscm-error 'write-image "Saving of typecode ~a not supported" (typecode img)))
    (if (not (eqv? (dimensions img) 2))
      (aiscm-error 'write-image "Image must have 2 dimensions but had ~a" (dimensions img)))
    (magick-write-image format (shape compacted) (memory compacted) file-name)
    img))
