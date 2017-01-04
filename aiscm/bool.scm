;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016 Jan Wedekind <jan@wedesoft.de>
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
(define-module (aiscm bool)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)
  #:use-module (aiscm util)
  #:use-module (aiscm element)
  #:use-module (aiscm scalar)
  #:export (<bool> <meta<bool>>
            && || ! !=)
  #:re-export (=))
(define-class* <bool> <scalar> <meta<bool>> <meta<scalar>>)
(define-method (size-of (self <meta<bool>>)) 1)
(define-method (pack (self <bool>))
  (u8-list->bytevector (list (if (get self) 1 0))))
(define-method (unpack (self <meta<bool>>) (packed <bytevector>))
  (make <bool> #:value (if (eq? (car (bytevector->u8-list packed)) 0) #f #t)))
(define-method (coerce (a <meta<bool>>) (b <meta<bool>>)) <bool>)
(define-method (write (self <bool>) port)
  (format port "#<<bool> ~a>" (get self)))
(define-method (native-type (b <boolean>) . args) (if (every boolean? args) <bool> (next-method)))
(define-method (unbuild (type <meta<bool>>) self) (list (if self 1 0)))
(define-method (&& a) a)
(define-method (&& (a <boolean>) (b <boolean>)) (and a b))
(define-method (&& a b c . args) (apply && (&& (&& a b) c) args))
(define-method (|| a) a)
(define-method (|| (a <boolean>) (b <boolean>)) (or a b))
(define-method (|| a b c . args) (apply || (|| (|| a b) c) args))
(define-generic !=)
(define-generic =)
(define-method (! (a <boolean>)) (not a))
