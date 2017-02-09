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
(use-modules (srfi srfi-26)
             (srfi srfi-64)
             (system foreign)
             (oop goops)
             (aiscm obj)
             (aiscm element)
             (aiscm int)
             (aiscm float)
             (aiscm bool)
             (aiscm sequence)
             (aiscm jit)
             (aiscm asm)
             (aiscm util))


(test-begin "aiscm obj")

(define obj (make <obj> #:value 'sym))
(define address (scm->address 'sym))
(define ctx (make <context>))

(test-eqv "size of SCM reference is 64 bits"
  8 (size-of <obj>))
(test-equal "pack Scheme object"
  #vu8(#xaa #x00 #x00 #x00 #x00 #x00 #x00 #x00) (pack (make <obj> #:value 42)))
(test-equal "unpack Scheme object"
  (make <obj> #:value 42) (unpack <obj> #vu8(#xaa #x00 #x00 #x00 #x00 #x00 #x00 #x00)))
(test-eq "objects coerce to objects"
  <obj> (coerce <obj> <obj>))
(test-eq "object and integer coerce to object"
  <obj> (coerce <obj> <int>))
(test-eq "integer and object coerce to object"
  <obj> (coerce <int> <obj>))
(test-equal "write wrapped object"
  "#<<obj> abc>" (call-with-output-string (lambda (port) (write (make <obj> #:value 'abc) port))))
(test-eq "native type for a symbol is <obj>"
  <obj> (native-type 'a))
(test-equal "build SCM value"
  'sym (build <obj> (list 'sym)))
(test-equal "content of symbol returns internal 64 bit representation"
  (list address) (unbuild <obj> 'sym))
(test-eq "compile and run call to scm_difference with one argument"
  -300 ((jit ctx (list <obj>) (cut make-native-function obj-negate <>)) 300))
(test-eq "compile and run bitwise not"
  -124 ((jit ctx (list <obj>) (cut make-native-function scm-lognot <>)) 123))
(test-equal "compile and run comparison with zero"
  '(#f #t) (map (jit ctx (list <obj>) (cut make-native-function obj-zero-p <>)) '(3 0)))
(test-equal "compile and run not-equal-to zero"
  '(#t #f) (map (jit ctx (list <obj>) (cut make-native-function obj-nonzero-p <>)) '(3 0)))
(test-equal "compile logical not for Scheme objects"
  '(#t #f #f #f #f) (map (jit ctx (list <obj>) (cut make-native-function obj-not <>)) '(#f #t () 0 1)))
(test-eq "compile and run call to scm_sum"
  300 ((jit ctx (list <obj> <obj>) (cut make-native-function scm-sum <...>)) 100 200))
(test-eq "compile and run call to scm_difference"
  100 ((jit ctx (list <obj> <obj>) (cut make-native-function scm-difference <...>)) 300 200))
(test-eq "compile and run call to scm_product"
  600 ((jit ctx (list <obj> <obj>) (cut make-native-function scm-product <...>)) 20 30))
(test-eq "compile and run call to scm_divide"
  30 ((jit ctx (list <obj> <obj>) (cut make-native-function scm-divide <...>)) 600 20))
(test-eq "compile and run call to scm_remainder"
  33 ((jit ctx (list <obj> <obj>) (cut make-native-function scm-remainder <...>)) 123 45))
(test-eq "compile and run call to scm_logand"
  72 ((jit ctx (list <obj> <obj>) (cut make-native-function scm-logand <...>)) 123 456))
(test-eq "compile and run call to scm_logior"
  507 ((jit ctx (list <obj> <obj>) (cut make-native-function scm-logior <...>)) 123 456))
(test-eq "compile and run call to scm_logxor"
  435 ((jit ctx (list <obj> <obj>) (cut make-native-function scm-logxor <...>)) 123 456))
(test-equal "compile logical and for Scheme objects"
  '(#f b) (map (jit ctx (list <obj> <obj>) (cut make-native-function obj-and <...>)) '(#f a) '(b b)))
(test-equal "compile logical or for Scheme objects"
  '(b a) (map (jit ctx (list <obj> <obj>) (cut make-native-function obj-or <...>)) '(#f a) '(b b)))
(test-eq "compile and run call to scm_min"
  123 ((jit ctx (list <obj> <obj>) (cut make-native-function scm-min <...>)) 123 456))
(test-eq "compile and run call to scm_max"
  456 ((jit ctx (list <obj> <obj>) (cut make-native-function scm-max <...>)) 123 456))
(test-eq "compile and run call to scm_ash"
  1968 ((jit ctx (list <obj> <obj>) (cut make-native-function scm-ash <...>)) 123 4))
(test-eq "compile and run call to obj_shr"
  123 ((jit ctx (list <obj> <obj>) (cut make-native-function obj-shr <...>)) 1968 4))
(test-equal "compile and run equality of objects"
  '(#f #t) (map (jit ctx (list <obj> <obj>) (cut make-native-function obj-equal-p <...>)) '(21 42) '(42 42)))
(test-equal "compile and run inequality of objects"
  '(#t #f) (map (jit ctx (list <obj> <obj>) (cut make-native-function obj-nequal-p <...>)) '(21 42) '(42 42)))
(test-equal "compile and run lower-than comparison for objects"
  '(#t #f #f) (map (jit ctx (list <obj> <obj>) (cut make-native-function obj-less-p <...>)) '(3 5 7) '(5 5 5)))
(test-equal "compile and run lower-equal comparison for objects"
  '(#t #t #f) (map (jit ctx (list <obj> <obj>) (cut make-native-function obj-leq-p <...>)) '(3 5 7) '(5 5 5)))
(test-equal "compile and run greater-than comparison for objects"
  '(#f #f #t) (map (jit ctx (list <obj> <obj>) (cut make-native-function obj-gr-p <...>)) '(3 5 7) '(5 5 5)))
(test-equal "compile and run greater-equal comparison for objects"
  '(#f #t #t) (map (jit ctx (list <obj> <obj>) (cut make-native-function obj-geq-p <...>)) '(3 5 7) '(5 5 5)))
(test-assert "object meory is not pointerless"
  (not (pointerless? <obj>)))
(test-assert "object references don't need sign-extension"
  (not (signed? <obj>)))
(test-equal "compile and run call to obj_from_bool"
  '(#f #t) (map (jit ctx (list <bool>) (cut make-native-function obj-from-bool <>)) '(#f #t)))
(test-equal "compile and run call to scm_to_bool"
  '(#f #t) (map (jit ctx (list <obj>) (cut make-native-function scm-to-bool <>)) '(#f #t)))
(test-eqv "compile and run call to scm_to_int8"
  123 ((jit ctx (list <obj>) (cut make-native-function scm-to-uint8 <>)) 123))
(test-eqv "compile and run call to scm_from_int8"
  123 ((jit ctx (list <ubyte>) (cut make-native-function scm-from-uint8 <>)) 123))
(test-eqv "compile and run call to scm_to_int8"
  -123 ((jit ctx (list <obj>) (cut make-native-function scm-to-int8 <>)) -123))
(test-eqv "compile and run call to scm_from_int8"
  -123 ((jit ctx (list <byte>) (cut make-native-function scm-from-int8 <>)) -123))
(test-eqv "compile and run call to scm_to_int16"
  123 ((jit ctx (list <obj>) (cut make-native-function scm-to-uint16 <>)) 123))
(test-eqv "compile and run call to scm_from_int16"
  123 ((jit ctx (list <usint>) (cut make-native-function scm-from-uint16 <>)) 123))
(test-eqv "compile and run call to scm_to_int16"
  -123 ((jit ctx (list <obj>) (cut make-native-function scm-to-int16 <>)) -123))
(test-eqv "compile and run call to scm_from_int16"
  -123 ((jit ctx (list <sint>) (cut make-native-function scm-from-int16 <>)) -123))
(test-eqv "compile and run call to scm_to_int32"
  123 ((jit ctx (list <obj>) (cut make-native-function scm-to-uint32 <>)) 123))
(test-eqv "compile and run call to scm_from_int32"
  123 ((jit ctx (list <uint>) (cut make-native-function scm-from-uint32 <>)) 123))
(test-eqv "compile and run call to scm_to_int32"
  -123 ((jit ctx (list <obj>) (cut make-native-function scm-to-int32 <>)) -123))
(test-eqv "compile and run call to scm_from_int32"
  -123 ((jit ctx (list <int>) (cut make-native-function scm-from-int32 <>)) -123))
(test-eqv "compile and run call to scm_to_int64"
  123 ((jit ctx (list <obj>) (cut make-native-function scm-to-uint64 <>)) 123))
(test-eqv "compile and run call to scm_from_int64"
  123 ((jit ctx (list <ulong>) (cut make-native-function scm-from-uint64 <>)) 123))
(test-eqv "compile and run call to scm_to_int64"
  -123 ((jit ctx (list <obj>) (cut make-native-function scm-to-int64 <>)) -123))
(test-eqv "compile and run call to scm_from_int64"
  -123 ((jit ctx (list <long>) (cut make-native-function scm-from-int64 <>)) -123))
(test-equal "implicit conversion from long integer to object"
  -42 ((jit ctx (list <long>) (cut make-native-function obj-negate <>)) 42))

(test-end "aiscm obj")
