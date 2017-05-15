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
(define-module (aiscm expression)
  #:use-module (oop goops)
  #:use-module (aiscm element)
  #:use-module (aiscm int)
  #:use-module (aiscm sequence)
  #:use-module (aiscm variable)
  #:export (<param> <lookup> <indexer>
            skeleton delegate parameter lookup indexer index))


(define-method (skeleton (self <meta<element>>)) (make self #:value (var self)))
(define-method (skeleton (self <meta<element>>)) (make self #:value (var self)))
(define-method (skeleton (self <meta<sequence<>>>))
  (let [(slice (skeleton (project self)))]
    (make self
          #:value   (value slice)
          #:shape   (cons (var <long>) (shape   slice))
          #:strides (cons (var <long>) (strides slice)))))

(define-class <param> ()
  (delegate #:init-keyword #:delegate #:getter delegate))

(define-method (parameter (self <element>)) (make <param> #:delegate self))
(define-method (parameter (self <sequence<>>))
  (let [(idx (var <long>))]
    (indexer idx
             (lookup idx
                     (parameter (project self))
                     (parameter (make <long> #:value (stride self))))
             (parameter (make <long> #:value (dimension self))))))
(define-method (parameter (self <meta<element>>)) (parameter (skeleton self)))

(define-class <indexer> (<param>)
  (dimension #:init-keyword #:dimension #:getter dimension)
  (index     #:init-keyword #:index     #:getter index))
(define (indexer index delegate dimension)
  (make <indexer> #:dimension dimension #:index index #:delegate delegate))

(define-class <lookup> (<param>)
  (index    #:init-keyword #:index    #:getter index   )
  (stride   #:init-keyword #:stride   #:getter stride  ))
(define-method (lookup index delegate stride)
  (make <lookup> #:index index #:delegate delegate #:stride stride))
(define-method (lookup idx (obj <indexer>) stride)
  (indexer (index obj) (lookup idx (delegate obj) stride) (dimension obj)))
