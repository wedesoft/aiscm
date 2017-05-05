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
(define-module (aiscm variable)
  #:use-module (oop goops)
  #:use-module (aiscm element)
  #:use-module (aiscm pointer)
  #:use-module (aiscm bool)
  #:use-module (aiscm int)
  #:use-module (aiscm float)
  #:use-module (aiscm obj)
  #:use-module (aiscm asm)
  #:use-module (aiscm util)
  #:export (<var> <ptr>
            var native-equivalent variables get-args)
  #:re-export (typecode))


(define-method (get-args self) '())

(define-class <var> ()
  (type   #:init-keyword #:type   #:getter typecode)
  (symbol #:init-keyword #:symbol #:init-form (gensym)))
(define-method (write (self <var>) port)
  (format port "~a:~a" (symbol->string (slot-ref self 'symbol)) (class-name (slot-ref self 'type))))
(define-method (size-of (self <var>)) (size-of (typecode self)))
(define-method (var self) (make <var> #:type (native-equivalent self)))

(define-class <ptr> ()
  (type #:init-keyword #:type #:getter typecode)
  (args #:init-keyword #:args #:getter get-args))
(define-method (write (self <ptr>) port)
  (display (cons 'ptr (cons (class-name (typecode self)) (get-args self))) port))
(define-method (equal? (a <ptr>) (b <ptr>)) (equal? (object-slots a) (object-slots b)))
(define-method (ptr (type <meta<element>>) . args) (make <ptr> #:type type #:args args))

(define-method (native-equivalent  self                   ) #f      )
(define-method (native-equivalent (self <meta<bool>>     )) <ubyte> )
(define-method (native-equivalent (self <meta<int<>>>    )) self    )
(define-method (native-equivalent (self <meta<float<>>>  )) self    )
(define-method (native-equivalent (self <meta<obj>>      )) <ulong> )
(define-method (native-equivalent (self <meta<pointer<>>>)) <ulong> )

(define-method (variables self) '())
(define-method (variables (self <var>)) (list self))
(define-method (variables (self <ptr>)) (variables (get-args self)))
