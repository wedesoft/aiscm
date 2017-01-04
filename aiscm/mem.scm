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
(define-module (aiscm mem)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (aiscm element)
  #:use-module (aiscm util)
  #:export (<mem>
            get-memory
            read-bytes
            write-bytes))
(define-class <mem> ()
  (memory #:init-keyword #:memory #:getter get-memory)
  (base #:init-keyword #:base)
  (size #:init-value 0 #:init-keyword #:size #:getter get-size))
(define (align16 ptr) (make-pointer (logand (+ (pointer-address ptr) #xf) #x-10)))
(define-method (initialize (self <mem>) initargs)
  (let-keywords initargs #f (memory base size pointerless)
    (if base
      (next-method self (list #:memory (or memory base) #:base base #:size size))
      (let* [(malloc (if pointerless gc-malloc-pointerless gc-malloc))
             (ptr    (malloc (+ size 15)))]
        (next-method self (list #:memory (align16 ptr) #:base ptr #:size size))))))
(define-generic +)
(define-method (+ (self <mem>) (offset <integer>))
  (let [(size (get-size self))]
    (if (negative? offset)
      (aiscm-error '+ "Offset not be lower than zero but was ~a" offset)
      (make <mem>
            #:memory (make-pointer (+ offset (pointer-address (get-memory self))))
            #:base (slot-ref self 'base)
            #:size (- size offset)))))
(define-method (equal? (a <mem>) (b <mem>))
  (equal? (get-memory a) (get-memory b)))
(define-method (read-bytes (self <mem>) (size <integer>))
  (if (> size (get-size self))
      (aiscm-error 'read-bytes "Attempt to read ~a bytes from memory of size ~a" size (get-size self))
      (pointer->bytevector (get-memory self) size)))
(define-method (write-bytes (self <mem>) (bv <bytevector>))
  (bytevector-copy!
    bv 0
    (pointer->bytevector (get-memory self) (get-size self)) 0
    (bytevector-length bv)))
(define-method (write (self <mem>) port)
  (format port "#<<mem> #x~x ~a>"
          (pointer-address (get-memory self)) (get-size self)))
