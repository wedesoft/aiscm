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
(use-modules (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-64)
             (oop goops)
             (system foreign)
             (aiscm element)
             (aiscm sequence)
             (aiscm int)
             (aiscm bool)
             (aiscm obj)
             (aiscm rgb)
             (aiscm asm)
             (aiscm jit))


(test-begin "aiscm jit6")

(load-extension "libguile-aiscm-tests" "init_tests")

(define ctx (make <context>))

(let [(a (parameter <int>))]
  (test-equal "Passing register parameters creates copy instructions"
    (MOV EDI (get (delegate a))) (car (pass-parameters (list a) (NOP)))))
(let [(args (map parameter (make-list 7 <int>)))]
  (test-equal "Passing stack parameters pushes the parameters on the stack"
    (PUSH (get (delegate (list-ref args 6)))) (list-ref (pass-parameters args (NOP)) 6))
  (test-equal "Stack pointer gets corrected after stack parameters have been used"
    (ADD RSP #x08) (last (pass-parameters args (NOP)))))
(test-eq "Scheme objects are represented using unsigned 64-bit integers"
  <ulong> (typecode (var <obj>)))
(let [(o (skeleton <obj>))]
  (test-assert "skeleton of top object is of type obj"
    (is-a? o <obj>))
  (test-assert "value of object skeleton is a variable"
    (is-a? (value o) <var>))
  (test-eq "value of object skeleton is of type unsigned long integer"
    <ulong> (typecode (value o))))
(test-assert "do not decompose variables"
  (is-a? (car (content <obj> (var <long>))) <var>))
(test-eq "compile and run identity function accepting Scheme object"
  'symbol ((jit ctx (list <obj>) identity) 'symbol))
(test-eq "make sure \"content\" enforces SCM object arguments"
  42 ((jit ctx (list <obj>) identity) 42))
(test-eq "negation of Scheme object"
  -300 ((jit ctx (list <obj>) -) 300))
(test-eq "bitwise logical not using Scheme objects"
  -124 ((jit ctx (list <obj>) ~) 123))
(test-equal "comparison of Scheme object with zero"
  '(#f #t) (map (jit ctx (list <obj>) =0) '(3 0)))
(test-equal "Scheme object not equal to zero"
  '(#t #f) (map (jit ctx (list <obj>) !=0) '(3 0)))
(test-equal "compiled logical not for Scheme objects"
  '(#t #f #f #f #f) (map (jit ctx (list <obj>) !) '(#f #t () 0 1)))
(test-eq "compiled plus operation using Scheme objects"
  300 ((jit ctx (list <obj> <obj>) +) 100 200))
(test-eq "compiled unary plus using Scheme objects"
  300 ((jit ctx (list <obj>) +) 300))
(test-eq "compiled minus operation using Scheme objects"
  100 ((jit ctx (list <obj> <obj>) -) 300 200))
(test-eq "compiled multiplication using Scheme objects"
  600 ((jit ctx (list <obj> <obj>) *) 20 30))
(test-eq "compiled division using Scheme objects"
  5 ((jit ctx (list <obj> <obj>) /) 15 3))
(test-eq "compiled modulo using Scheme objects"
  33 ((jit ctx (list <obj> <obj>) %) 123 45))
(test-eq "bitwise and using Scheme objects"
  72 ((jit ctx (list <obj> <obj>) &) 123 456))
(test-eq "bitwise or using Scheme objects"
  507 ((jit ctx (list <obj> <obj>) |) 123 456))
(test-eq "bitwise exclusive-or using Scheme objects"
  435 ((jit ctx (list <obj> <obj>) ^) 123 456))
(test-equal "logical and for Scheme objects"
  '(#f b) (map (jit ctx (list <obj> <obj>) &&) '(#f a) '(b b)))
(test-equal "logical or for Scheme objects"
  '(b a) (map (jit ctx (list <obj> <obj>) ||) '(#f a) '(b b)))
(test-eq "compiled minimum using Scheme objects"
  123 ((jit ctx (list <obj> <obj>) min) 123 456))
(test-eq "compiled maximum using Scheme objects"
  456 ((jit ctx (list <obj> <obj>) max) 123 456))
(test-eq "compiled shift-left using Scheme objects"
  1968 ((jit ctx (list <obj> <obj>) <<) 123 4))
(test-eq "compiled shift-right using Scheme objects"
  123 ((jit ctx (list <obj> <obj>) >>) 1968 4))
(test-equal "compiled equal comparison of Scheme objects"
  (list #f #t) (map (jit ctx (list <obj> <obj>) =) '(21 42) '(42 42)))
(test-equal "compiled unequal comparison of Scheme objects"
  (list #t #f) (map (jit ctx (list <obj> <obj>) !=) '(21 42) '(42 42)))
(test-equal "compiled lower-than comparison for Scheme objects"
  (list #t #f #f) (map (jit ctx (list <obj> <obj>) <) '(3 5 7) '(5 5 5)))
(test-equal "compiled lower-equal comparison for Scheme objects"
  (list #t #t #f) (map (jit ctx (list <obj> <obj>) <=) '(3 5 7) '(5 5 5)))
(test-equal "compiled greater-than comparison for Scheme objects"
  (list #f #f #t) (map (jit ctx (list <obj> <obj>) >) '(3 5 7) '(5 5 5)))
(test-equal "compiled greater-equal comparison for Scheme objects"
  (list #f #t #t) (map (jit ctx (list <obj> <obj>) >=) '(3 5 7) '(5 5 5)))
(test-eq "convert Scheme object to integer in compiled code"
  42 ((jit ctx (list <obj>) (cut to-type <int> <>)) 42))
(test-equal "compile and run identity function for sequence of objects"
  '(2 3 5) (to-list ((jit ctx (list (sequence <obj>)) identity) (seq <obj> 2 3 5))))
(test-equal "dereference pointer when doing bitwise negation"
  '(-3 -4 -6) (to-list ((jit ctx (list (sequence <obj>)) ~) (seq <obj> 2 3 5))))
(test-equal "convert unsigned byte to boolean"
  '(#f #t) (map (jit ctx (list <ubyte>) (cut to-type <bool> <>)) '(0 1)))
(test-equal "convert integer to boolean"
  '(#f #t) (map (jit ctx (list <int>) (cut to-type <bool> <>)) '(0 1)))
(test-equal "convert boolean to unsigned byte"
  '(0 1) (map (jit ctx (list <bool>) (cut to-type <ubyte> <>)) '(#f #t)))
(test-eqv "convert integer to object"
  42 ((jit ctx (list <int>) (cut to-type <obj> <>)) 42))
(test-equal "convert object to boolean"
  '(#f #t) (map (jit ctx (list <obj>) (cut to-type <bool> <>)) '(#f #t)))
(test-equal "convert boolean to object"
  '(#f #t) (map (jit ctx (list <bool>) (cut to-type <obj> <>)) '(#f #t)))
(test-equal "convert integer RGB sequence to object RGB sequence"
  (list (rgb 2 3 5)) (to-list (to-type (rgb <obj>) (seq (rgb 2 3 5)))))
(test-equal "convert object RGB sequence to integer RGB sequence"
  (list (rgb 2 3 5)) (to-list (to-type (rgb <int>) (seq (rgb <obj>) (rgb 2 3 5)))))
(test-eqv "plus for object and integer"
  7 ((jit ctx (list <obj> <int>) +) 3 4))
(test-eqv "plus for integer and object"
  7 ((jit ctx (list <int> <obj>) +) 3 4))

(test-end "aiscm jit6")
