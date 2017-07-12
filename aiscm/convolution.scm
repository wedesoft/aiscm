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
(define-module (aiscm convolution)
  #:use-module (oop goops)
  #:use-module (aiscm element)
  #:use-module (aiscm asm)
  #:use-module (aiscm jit)
  #:use-module (aiscm expression)
  #:export (convolve))


(define ctx (make <context>))

(define-method (convolve data kernel)
  (convolve data (wrap kernel))); TODO: wrap data?

(define-method (convolve (data <element>) (kernel <element>))
  (let* [(types (map class-of (list data kernel)))
         (f     (jit ctx types convolution))]
    (add-method! convolve
                 (make <method>
                       #:specializers types
                       #:procedure (lambda (data kernel) (f data (get kernel)))))
    (convolve data kernel)))
