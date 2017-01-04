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
(define-module (aiscm element)
  #:use-module (oop goops)
  #:use-module (aiscm util)
  #:use-module (system foreign)
  #:export (<element>
            <meta<element>>
            value get set size-of foreign-type pack unpack channels rate
            typecode size shape strides dimensions coerce native-type wrap get-size
            build content unbuild component base to-type duplicate read-image read-audio
            write-image write-audio pointerless?))
(define-class* <element> <object> <meta<element>> <class>
               (value #:init-keyword #:value #:getter value))
(define-method (size-of (self <element>)) (size-of (class-of self)))
(define-method (foreign-type (t <class>)) void)
(define-generic pack)
(define-generic unpack)
(define-generic channels)
(define-generic rate)
(define-method (equal? (a <element>) (b <element>)) (equal? (get a) (get b)))
(define-method (size (self <element>)) 1)
(define-method (shape self) '())
(define-method (strides self) '())
(define-method (dimensions (self <meta<element>>)) 0)
(define-method (dimensions (self <element>)) (dimensions (class-of self)))
(define-method (typecode (self <meta<element>>)) self)
(define-method (typecode (self <element>)) (typecode (class-of self)))
(define-method (get (self <element>)) (value self))
(define-method (set (self <element>) value) (begin (slot-set! self 'value value)) value)
(define-generic slice)
(define-generic coerce)
(define-generic native-type)
(define-method (wrap self) (make (native-type self) #:value self))
(define-method (wrap (self <element>)) self)
(define-generic get-size)
(define-method (build self value) (car value))
(define-method (content (type <meta<element>>) self) (list self))
(define-method (unbuild (type <meta<element>>) self) (list self))
(define-method (component type self offset) self)
(define-method (base self) self)
(define-generic to-type)
(define-generic duplicate)
(define-generic read-image)
(define-generic read-audio)
(define-generic write-image)
(define-generic write-audio)
(define-method (pointerless? self) #t)
