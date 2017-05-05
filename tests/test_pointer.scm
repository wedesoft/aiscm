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
(use-modules (srfi srfi-64)
             (oop goops)
             (system foreign)
             (ice-9 regex)
             (aiscm element)
             (aiscm pointer)
             (aiscm variable)
             (aiscm mem)
             (aiscm bool)
             (aiscm int)
             (aiscm obj)
             (aiscm jit))


(test-begin "aiscm pointer")

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

(test-equal "equal pointer types"
  (pointer <bool>) (pointer <bool>))
(test-equal "equal pointer types"
  (pointer <byte>) (pointer (integer 8 signed)))
(test-equal "equal pointers"
  p1-bool (make (pointer <bool>) #:value m1))
(test-assert "unequal pointers"
  (not (equal? p1-bool p2-bool)))
(test-assert "unequal pointers (different type)"
  (not (equal? p1-bool p1-byte)))
(test-equal "display short integer object"
  "#<<pointer<int<32,signed>>> p:<int<64,signed>>>"
  (call-with-output-string (lambda (port) (display (make (pointer <int>) #:value p) port))))
(test-equal "fetch boolean from memory"
  (make <bool> #:value #t) (fetch p1-bool))
(test-equal "fetch byte from memory"
  (make <byte> #:value 1) (fetch p1-byte))
(test-equal "fetch short integer from memory"
  (make <sint> #:value #x0201) (fetch p1-sint))
(test-equal "storing and fetching back short int"
  (make <sint> #:value #x0201) (begin (store p2-sint #x0201) (fetch p2-sint)))
(test-equal "pointer operations are aware of size of element"
  (+ m2 2) (get (+ p2-sint 1)))
(test-eqv "convert pointer to bytevector containing raw data"
  (pointer-address (get-memory m1)) (get (unpack <native-int> (pack p1-byte))))
(test-assert "write pointer object"
  (string-match "^#<<pointer<int<16,signed>>> .*>$"
  (call-with-output-string (lambda (port) (write p1-sint port)))))
(test-assert "display pointer object"
  (string-match "^#<<pointer<int<16,signed>>> .*>$"
  (call-with-output-string (lambda (port) (display p1-sint port)))))
(test-eqv "Memory is allocated if no value is specified"
  4 (get-size (get (make (pointer <int>)))))
(test-equal "Content of pointer value is the address as a number"
  (list (pointer-address (get-memory m1))) (unbuild <pointer<>> p1-byte))
(test-equal "Content of pointer is a list with the content of the pointer as an integer"
  (list (make <ulong> #:value p)) (content (pointer <int>) (make (pointer <byte>) #:value p)))
(let [(p (parameter (pointer <int>)))]
  (test-equal "content of pointer is based on same value"
  (get (delegate p)) (get (delegate (car (content (pointer <int>) p))))))
(let [(v (var <long>))]
  (test-equal "rebase a pointer"
  v (get (rebase v (make (pointer <byte>) #:value p)))))
(test-eq "casting pointer preserves address"
  m1 (get (pointer-cast <int> p1-sint)))
(test-eq "casting pointer changes type"
  <int> (typecode (pointer-cast <int> p1-sint)))
(test-assert "Pointer has no offset by default"
  (not (pointer-offset p1-bool)))
(let* [(p (make (pointer <int>) #:value m1))
       (q (set-pointer-offset p 123))]
  (test-eq "Associating pointer offset maintains value"
    m1 (get q))
  (test-eq "Associated offset can be queried"
    123 (pointer-offset q))
  (test-assert "Original pointer remains without offset"
  (not (pointer-offset p))))
(test-assert "integer pointer memory is pointerless"
  (pointerless? (pointer <int>)))
(test-assert "object pointer  memory is not pointerless"
  (not (pointerless? (pointer <obj>))))

(test-end "aiscm pointer")
