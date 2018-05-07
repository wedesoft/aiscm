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
(use-modules (srfi srfi-64)
             (oop goops)
             (ice-9 regex)
             (system foreign)
             (aiscm element)
             (aiscm mem))


(test-begin "aiscm mem")

(define m (make <mem> #:size 10))

(test-assert "memory pointer should be greater or equal base pointer"
  (>= (pointer-address (get-memory m)) (pointer-address (slot-ref m 'base))))
(test-equal "default value for memory slot is value of base"
  (get-memory m) (get-memory (make <mem> #:base (get-memory m) #:size 10)))
(test-eqv "'get-size' returns size of allocated memory"
  10 (get-size m))
(test-equal "base pointer is copied when creating pointer with offset"
  (slot-ref m 'base) (slot-ref (+ m 6) 'base))
(test-equal "equal mem objects"
  (+ m 1) (+ m 1))
(test-assert "unequal mem objects"
  (not (equal? (+ m 1) (+ m 2))))
(test-eqv "pointer operations keep track of memory size"
  4 (get-size (+ m 6)))
(test-error "throw exception when pointer offset is negative"
  'misc-error (+ m -1))
(test-equal "writing and reading to/from memory"
  #vu8(2 3 5) (begin (write-bytes m #vu8(2 3 5 7)) (read-bytes m 3)))
(test-equal "writing and reading with offset to/from memory"
  #vu8(3 5 7) (begin (write-bytes m #vu8(2 3 5 7)) (read-bytes (+ m 1) 3)))
(test-equal "writing with overlap and reading back"
  #vu8(2 2 1 1) (begin (write-bytes m #vu8(1 1 1 1)) (write-bytes m #vu8(2 2)) (read-bytes m 4)))
(test-error "throw exception when reading past memory boundary"
  'misc-error (read-bytes m 11))
(test-error "throw exception when attempting to write past memory boundary"
  'misc-error (write-bytes m #vu8(1 2 3 4 5 6 7 8 9 10 11)))
(test-assert "display mem object"
  (string-match "^#<<mem> #x[0-9a-f]* 10>$" (call-with-output-string (lambda (port) (display m port)))))
(test-assert "write mem object"
  (string-match "^#<<mem> #x[0-9a-f]* 10>$" (call-with-output-string (lambda (port) (write m port)))))

(test-end "aiscm mem")
