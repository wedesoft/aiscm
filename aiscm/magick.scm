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
(define-module (aiscm magick)
  #:use-module (oop goops)
  #:use-module (aiscm util)
  #:use-module (aiscm mem)
  #:use-module (aiscm element)
  #:use-module (aiscm pointer)
  #:use-module (aiscm rgb)
  #:use-module (aiscm int)
  #:use-module (aiscm sequence)
  #:use-module (aiscm jit)
  #:re-export (<pointer<element>> <meta<pointer<element>>>
               <pointer<rgb<>>> <meta<pointer<rgb>>>>
               read-image write-image))

(load-extension "libguile-aiscm-magick" "init_magick")
(define-method (read-image (file-name <string>))
  (let [(picture    (magick-read-image file-name))
        (array-type (lambda (format) (multiarray (if (eq? format 'I) <ubyte> <ubytergb>) 2)))
        (memory     (lambda (base mem size) (make <mem> #:base base #:memory mem #:size size)))
        (array      (lambda (array-type shape memory) (make array-type #:shape shape #:value memory)))]
    (apply (lambda (format shape base mem size)
             (array (array-type format) shape (memory base mem size)))
           picture)))
(define-method (write-image (img <sequence<>>) (file-name <string>))
  (let [(format    (cond ((eq? (typecode img) <ubyte>)    'I)
                         ((eq? (typecode img) <ubytergb>) 'RGB)
                         (else #f)))
        (compacted (ensure-default-strides img))]
    (if (not format)
      (aiscm-error 'write-image "Saving of typecode ~a not supported" (typecode img)))
    (if (not (eqv? (dimensions img) 2))
      (aiscm-error 'write-image "Image must have 2 dimensions but had ~a" (dimensions img)))
    (magick-write-image format (shape compacted) (get-memory (value compacted)) file-name)
    img))
