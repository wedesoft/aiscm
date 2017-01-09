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
(use-modules (oop goops)
             (system foreign)
             (ice-9 regex)
             (aiscm element)
             (aiscm pointer)
             (aiscm mem)
             (aiscm bool)
             (aiscm int)
             (aiscm obj)
             (aiscm jit)
             (guile-tap))
(define p (make <var> #:type <long> #:symbol 'p))
(define m1 (make <mem> #:size 10))
(define m2 (make <mem> #:size 4))
(define p1-bool (make (pointer <bool>) #:value m1))
(define p2-bool (make (pointer <bool>) #:value m2))
(define p1-byte (make (pointer <byte>) #:value m1))
(define p2-byte (make (pointer <byte>) #:value m2))
(define p1-sint (make (pointer <sint>) #:value m1))
(define p2-sint (make (pointer <sint>) #:value m2))
(write-bytes m1 #vu8(1 2 3 4 5 6 7 8 9 10))
(write-bytes m2 #vu8(0 0 0 0))
(ok (equal? (pointer <bool>) (pointer <bool>))
    "equal pointer types")
(ok (equal? (pointer <byte>) (pointer (integer 8 signed)))
    "equal pointer types")
(ok (equal? p1-bool (make (pointer <bool>) #:value m1))
    "equal pointers")
(ok (not (equal? p1-bool p2-bool))
    "unequal pointers")
(ok (not (equal? p1-bool p1-byte))
    "unequal pointers (different type)")
(ok (equal? "#<<pointer<int<32,signed>>> p>"
            (call-with-output-string (lambda (port) (display (make (pointer <int>) #:value p) port))))
    "display short integer object")
(ok (equal? (make <bool> #:value #t) (fetch p1-bool))
    "fetch boolean from memory")
(ok (equal? (make <byte> #:value 1) (fetch p1-byte))
    "fetch byte from memory")
(ok (equal? (make <sint> #:value #x0201) (fetch p1-sint))
    "fetch short integer from memory")
(ok (equal? (make <sint> #:value #x0201)
            (begin (store p2-sint #x0201) (fetch p2-sint)))
    "storing and fetching back short int")
(ok (equal? (+ m2 2) (get (+ p2-sint 1)))
    "pointer operations are aware of size of element")
(ok (eqv? (pointer-address (get-memory m1))
          (get (unpack <native-int> (pack p1-byte))))
    "convert pointer to bytevector containing raw data")
(ok (string-match "^#<<pointer<int<16,signed>>> .*>$"
                  (call-with-output-string (lambda (port) (write p1-sint port))))
    "write pointer object")
(ok (string-match "^#<<pointer<int<16,signed>>> .*>$"
                  (call-with-output-string (lambda (port) (display p1-sint port))))
    "display pointer object")
(ok (eqv? 4 (get-size (get (make (pointer <int>)))))
    "Memory is allocated if no value is specified")
(ok (equal? (list (pointer-address (get-memory m1))) (unbuild <pointer<>> p1-byte))
    "Content of pointer value is the address as a number")
(ok (equal? (list (make <ulong> #:value p)) (content (pointer <int>) (make (pointer <byte>) #:value p)))
    "Content of pointer is a list with the content of the pointer as an integer")
(let [(p (parameter (pointer <int>)))]
  (ok (equal? (get (delegate p)) (get (delegate (car (content (pointer <int>) p)))))
      "content of pointer is based on same value"))
(let [(v (var <long>))]
  (ok (equal? v (get (rebase v (make (pointer <byte>) #:value p))))
      "rebase a pointer"))
(ok (eq? m1 (get (pointer-cast <int> p1-sint)))
    "casting pointer preserves address")
(ok (eq? <int> (typecode (pointer-cast <int> p1-sint)))
    "casting pointer changes type")
(ok (not (pointer-offset p1-bool))
    "Pointer has no offset by default")
(let* [(p (make (pointer <int>) #:value m1))
       (q (set-pointer-offset p 123))]
  (ok (eq? m1 (get q))
      "Associating pointer offset maintains value")
  (ok (eq? 123 (pointer-offset q))
      "Associated offset can be queried")
  (ok (not (pointer-offset p))
      "Original pointer remains without offset"))
(ok (pointerless? (pointer <int>))
    "integer pointer memory is pointerless")
(ok (not (pointerless? (pointer <obj>)))
    "object pointer  memory is not pointerless")
(run-tests)
