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
(use-modules (srfi srfi-1)
             (srfi srfi-64)
             (oop goops)
             (system foreign)
             (rnrs bytevectors)
             (aiscm util))


(test-begin "aiscm util")

(load-extension "libguile-aiscm-tests" "init_tests")

(test-begin "helper methods")
  (test-assert "Convert first element of Scheme array to integer"
    (scm-to-int-array-one-element '(123)))
  (test-assert "Convert second element of Scheme array to integer"
    (scm-to-int-array-second-element '(123 456)))
  (test-assert "Convert first element of Scheme array to long integer"
    (scm-to-long-array-one-element (list (ash 123 32))))
  (test-assert "Convert second element of Scheme array to long integer"
    (scm-to-long-array-second-element (list (ash 123 32) (ash 456 32))))
  (test-eqv "Call Scheme function without exception occurring"
    42 (call-scheme-function (lambda (arg) (car arg)) '(42)))
  (test-assert "Clean up object when exception occurs"
    (cleanup-when-exception (lambda (arg) (/ 1 0))))
  (test-error "Throw exception after cleaning up object"
    'misc-error (throw-exception-after-cleanup (lambda (arg) (/ 1 0))))
(test-end "helper methods")

(toplevel-define! 'a 0)
(define-class* <test<>> <object> <meta<test<>>> <class>
  (t #:init-keyword #:t #:getter get-t))
(template-class (test 32) <test<>>)
(template-class (test 8) <test<>>
  (lambda (class metaclass)
    (define-method (tplus8 (self class)) (+ 8 (get-t self)))
    (define-method (is8? (self metaclass)) #t)))
(define-class <values> ()
  (a #:init-keyword #:a)
  (b #:init-keyword #:b))
(test-eqv "'toplevel-define! should create a definition for the given symbol"
  0 a)
(test-eq "'super' returns the first direct superclass"
  <number> (super <complex>))
(test-eq "'define-class*' should define class and meta-class"
  <meta<test<>>> (class-of <test<>>))
(test-eq "'define-class*' creates the specified slots"
  42 (get-t (make <test<>> #:t 42)))
(test-eqv "'toplevel-define! should return the value of the definition"
  5 (toplevel-define! 'abc 5))
(test-eq "retrieve template class by it's arguments"
  <test<32>> (template-class (test 32) <test<>>))
(test-eq "meta class of template class"
  <meta<test<32>>> (class-of (template-class (test 32) <test<>>)))
(test-equal "base class of template class"
  <test<>> (super (template-class (test 32) <test<>>)))
(test-equal "base class of meta class of template class"
  <meta<test<>>> (super (class-of (template-class (test 32) <test<>>))))
(test-eq "class-name of template class"
  '<test<32>> (class-name <test<32>>))
(test-eq "class-name of template class with class arguments"
  '<test<pair>> (class-name (template-class (test <pair>) <test<>>)))
(test-eq "class-name of template class with multiple arguments"
  '<test<32,pair>> (class-name (template-class (test 32 <pair>) <test<>>)))
(test-eqv "template class can have methods"
  42 (tplus8 (make <test<8>> #:t 34)))
(test-assert "meta classes can have methods"
  (is8? <test<8>>))
(test-equal "exclusive-or for booleans"
  '(#f #t #t #f) (map xor '(#f #f #t #t) '(#f #t #f #t)))
(test-equal "'attach' should add an element at the end of the list"
  '(1 2 3) (attach '(1 2) 3))
(test-assert "'index' returns #f if value is not element of list"
  (not (index-of 4 '(2 3 5 7))))
(test-eqv "'index' returns index of first matching list element"
  2 (index-of 5 '(2 3 5 7)))
(test-equal "'all-but-last' should return a list with the last element removed"
  '(2 3 5) (all-but-last '(2 3 5 7)))
(test-assert "'drop-up-to' returns empty list if drop count is larger than length of list"
  (null? (drop-up-to '(1 2 3) 4)))
(test-equal "'drop-up-to' behaves like 'drop' otherwise"
  '(5 6) (drop-up-to '(1 2 3 4 5 6) 4))
(test-equal "'take-up-to' returns first elements"
  '(1 2 3) (take-up-to '(1 2 3 4 5) 3))
(test-equal "'take-up-to' returns all elements if list is smaller"
  '(1 2) (take-up-to '(1 2) 3))
(test-equal "'flatten' flattens a list"
  '(1 2 3 4) (flatten '(1 (2 3) ((4)))))
(test-equal "'cycle' should cycle the elements of a list"
  '(2 3 4 1) (cycle '(1 2 3 4)))
(test-equal "'uncycle' should reverse cycle the elements of a list"
  '(4 1 2 3) (uncycle '(1 2 3 4)))
(test-equal "cycling an array 0 times"
  '(1 2 3 4) (cycle-times '(1 2 3 4) 0))
(test-equal "cycling an array 0 times"
  '(1 2 3 4) (cycle-times '(1 2 3 4) 0))
(test-equal "cycling an array 1 times"
  '(2 3 4 1) (cycle-times '(1 2 3 4) 1))
(test-equal "cycling an array 2 times"
  '(3 4 1 2) (cycle-times '(1 2 3 4) 2))
(test-equal "cycling an array -1 times"
  '(4 1 2 3) (cycle-times '(1 2 3 4) -1))
(test-equal "cycling an array -2 times"
  '(3 4 1 2) (cycle-times '(1 2 3 4) -2))
(test-equal "'integral' should compute the accumulative sum of a list"
  '(1 3 6 10) (integral '(1 2 3 4)))
(test-equal "'alist-invert' should invert an association list"
  '((1 . a) (2 . b)) (alist-invert '((a . 1) (b . 2))))
(test-equal "'assq-set' should work with empty association list"
  '((3 . c)) (assq-set '() 3 'c))
(test-equal "'assq-set' should append new associations"
  '((1 . a) (2 . b) (3 . c)) (assq-set '((1 . a) (2 . b)) 3 'c))
(test-equal "'assq-set' should override old associations"
  '((1 . a) (2 . c)) (assq-set '((1 . a) (2 . b)) 2 'c))
(test-equal "'assq-remove' should remove entry with specified key"
  '((a . red) (c . blue)) (assq-remove '((a . red) (b . green) (c . blue)) 'b))
(test-equal "'assq-remove' should support removing multiple keys"
  '((c . blue)) (assq-remove '((a . red) (b . green) (c . blue)) 'b 'a))
(test-equal "'product' should create a product set of two lists"
  '((a . 1) (a . 2) (a . 3) (b . 1) (b . 2) (b . 3)) (product '(a b) '(1 2 3)))

(test-equal "'sort-by' should sort arguments by the values of the supplied function"
  '((a . 1) (b . 2) (c . 3)) (sort-by '((c . 3) (a . 1) (b . 2)) cdr))

(test-equal "'sort-by-pred' sorts by boolean result of predicate"
  '(1 3 5 0 2 4) (sort-by-pred (iota 6) even?))

(test-assert "partial sorting of empty list"
  (null? (partial-sort '() <)))
(test-equal "list of integers already sorted"
  '(2 3) (partial-sort '(2 3) <))
(test-equal "order list of two integers"
  '(3 5) (partial-sort '(5 3) <))
(test-equal "order list of two equal integers"
  '(5 5) (partial-sort '(5 5) <))
(test-equal "return items if order is not defined"
  '(7 3) (partial-sort '(7 3) (const #f)))
(test-equal "perform partial ordering"
  '(3 1 2) (partial-sort '(1 2 3) (lambda (x y) (eqv? x 3))))

(test-equal "Get element with minimum of argument"
  '(a . 1) (argmin cdr '((c . 3) (a . 1) (b . 2))))
(test-equal "Get element with maximum of argument"
  '(c . 3) (argmax cdr '((c . 3) (a . 1) (b . 2))))
(test-equal "'gather' groups elements into groups of specified size"
  '((0 1) (2 3 4) (5 6 7 8 9)) (gather '(2 3 5) (iota 10)))
(test-approximate "Fixed point iteration"
  (sqrt 2) (fixed-point 1 (lambda (x) (* 0.5 (+ (/ 2 x) x))) (lambda (a b) (< (abs (- a b)) 1e-5))) 1e-5)
(test-assert "'union' should merge two sets"
  (lset= eqv? '(1 2 3) (union '(1 2) '(2 3))))
(test-assert "'difference' should return the set difference"
  (lset= eqv? '(1) (difference '(1 2) '(2 3))))
(test-equal "Convert pair to list"
  '(a b) (pair->list '(a . b)))
(test-assert "Determine live intervals"
  (lset= equal?  '((a . (0 . 1)) (b . (1 . 2)) (c . (2 . 2))) (live-intervals '((a) (a b) (b c) ()) '(a b c))))
(test-equal "Determine variables with overlapping intervals for an interval"
  '((a b) (a b c) (b c))
  (map (overlap-interval '((a . (0 . 1)) (b . (1 . 2)) (c . (2 . 2)))) '((0 . 1) (1 . 2) (2 . 2))))
(test-equal "Return index of first element for given predicate"
  2 (first-index (lambda (x) (> x 4)) '(2 3 5 7 0)))
(test-assert "Return false if there is no element for given predicate"
  (not (first-index (lambda (x) (> x 7)) '(2 3 5 7 0))))
(test-equal "Return index of last element for given predicate"
  3 (last-index (lambda (x) (> x 4)) '(2 3 5 7 0)))
(test-assert "Return false if there is no element for given predicate"
  (not (last-index (lambda (x) (> x 7)) '(2 3 5 7 0))))
(test-equal "Remove false elements from arguments"
  '(1 2 3) (compact 1 2 #f 3 #f))
(test-equal "Extract part of byte vector"
  #vu8(3 5) (bytevector-sub #vu8(2 3 5 7 11) 1 2))
(test-equal "concatenate byte vectors"
  #vu8(2 3 5 7 11 13) (bytevector-concat (list #vu8(2 3) #vu8(5 7 11) #vu8(13))))
(test-equal "conditional map"
  '(1 -2 3 -4 5) (map-if even? - + '(1 2 3 4 5)))
(test-equal "conditional map with multiple arguments"
  '(1 5 8) (map-if (compose even? car list) - + '(2 3 5) '(1 2 3)))
(test-equal "selective map with multiple arguments"
  '(1 5 8) (map-select '(#t #f #f) - + '(2 3 5) '(1 2 3)))
(test-equal "generate 5 symbols"
  (make-list 5 #t) (map symbol? (symbol-list 5)))
(test-equal "Add type tags to method header"
  '((a <t1>) (b <t2>) (c <t3>)) (typed-header '(a b c) '(<t1> <t2> <t3>)))
(test-assert "Clock starts with zero"
  (let [(t (clock))] (>= (elapsed t) 0)))
(test-assert "Check clock has advanced after 1 second"
  (let [(t (clock))] (sleep 1) (>= (elapsed t) 1)))
(test-assert "Check clock has advanced after 100 milliseconds"
  (let [(t (clock))] (usleep 100000) (>= (elapsed t) 0.1)))
(test-assert "Check clock has not advanced too much after 100 milliseconds"
  (let [(t (clock))] (usleep 100000) (< (elapsed t) 0.2)))
(test-assert "Wait for one second"
  (let [(t (clock))] (synchronise #t 1 (compose sleep inexact->exact round)) (>= (elapsed t) 1)))
(test-assert "Check time with reset"
  (let [(t (clock))] (sleep 1) (elapsed t #t) (< (elapsed t) 1)))
(test-assert "Check time without reset"
  (let [(t (clock))] (sleep 1) (elapsed t) (>= (elapsed t) 1)))
(test-assert "Do not attempt to wait negative time"
  (synchronise #t -1 (compose sleep inexact->exact round)))
(test-eqv "Return specified result after synchronisation"
  42 (synchronise 42 0 identity))
(test-equal "Retrieve slot values from object"
  '(2 3) (object-slots (make <values> #:a 2 #:b 3)))

(test-begin "address conversions")
  (test-eq "convert Scheme object to address"
    (pointer-address (scm->pointer 123)) (scm->address 123))
  (test-eq "convert address to Scheme object"
    123 (address->scm (scm->address 123)))
(test-end "address conversions")

(test-equal "compute list with a value replaced"
  '(2 3 5 42 11) (list-with '(2 3 5 7 11) 3 42))

(test-begin "define typed methods")
  (define-typed-method typed-method (<integer> <integer>) -)
  (test-eqv "define a typed method for two integers"
    5 (typed-method 8 3))
  (test-error "do not define it for arbitrary types"
    'goops-error (typed-method "test" "other"))
(test-end "define typed methods")

(test-begin "define n-ary typed method")
  (define-nary-typed-method typed-unary-method 1 <integer> -)
  (test-eqv "define a unary typed method"
    -5 (typed-unary-method 5))
  (define-nary-typed-method typed-binary-method 2 <integer> -)
  (test-eqv "define a binary typed method"
    5 (typed-binary-method 8 3))
(test-end "define n-ary typed method")
(test-end "aiscm util")
