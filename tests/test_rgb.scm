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
             (srfi srfi-26)
             (aiscm element)
             (aiscm bool)
             (aiscm composite)
             (aiscm rgb)
             (aiscm int)
             (aiscm obj)
             (aiscm float)
             (aiscm sequence)
             (aiscm asm)
             (aiscm pointer)
             (aiscm jit))


(test-begin "aiscm rgb")

(define ctx (make <context>))
(define a (make <var> #:type <int> #:symbol 'a))
(define b (make <var> #:type <int> #:symbol 'b))
(define c (make <var> #:type <int> #:symbol 'c))
(define colour (make <ubytergb> #:value (rgb 1 2 3)))
(define grey (make <ubytergb> #:value 5))
(test-equal "display untyped RGB value"
  "(rgb 1 2 3)" (call-with-output-string (lambda (port) (write (rgb 1 2 3) port))))
(test-eq "equality of RGB types"
  (rgb (integer 8 unsigned)) (rgb (integer 8 unsigned)))
(test-eqv "storage size of unsigned byte RGB"
  3 (size-of (rgb <ubyte>)))
(test-eqv "storage size of single-precision floating-point RGB"
  12 (size-of (rgb (floating-point single-precision))))
(test-eq "base of RGB channel"
  <int> (base <intrgb>))
(test-equal "equal RGB objects"
  (rgb 1 2 3) (rgb 1 2 3))
(test-assert "unequal RGB objects"
  (not (equal? (rgb 1 2 3) (rgb 1 4 3))))
(test-eqv "extract red channel of RGB value"
  2 (red (rgb 2 3 5)))
(test-eqv "extract green channel of RGB value"
  3 (green (rgb 2 3 5)))
(test-eqv "extract blue channel of RGB value"
  5 (blue (rgb 2 3 5)))
(test-eqv "red channel of scalar is itself"
  42 (red 42))
(test-eqv "green channel of scalar is itself"
  42 (green 42))
(test-eqv "blue channel of scalar is itself"
  42 (blue 42))
(test-equal "pack RGB value"
  #vu8(#x01 #x02 #x03) (pack colour))
(test-equal "pack grey RGB value"
  #vu8(#x05 #x05 #x05) (pack grey))
(test-equal "unpack RGB value"
  colour (unpack <ubytergb> #vu8(#x01 #x02 #x03)))
(test-assert "RGB has no dimensions"
  (null? (shape colour)))
(test-equal "display short integer RGB object"
  "#<<rgb<int<16,signed>>> (rgb 1 2 3)>"
  (call-with-output-string (lambda (port) (display (make <sintrgb> #:value (rgb 1 2 3)) port))))
(test-equal "write short integer RGB object"
  "#<<rgb<int<16,signed>>> (rgb 1 2 3)>"
  (call-with-output-string (lambda (port) (write (make <sintrgb> #:value (rgb 1 2 3)) port))))
(test-eq "coerce RGB and scalar type"
  <ubytergb> (coerce <ubytergb> <ubyte>))
(test-eq "coerce scalar type and RGB"
  <ubytergb> (coerce <ubyte> <ubytergb>))
(test-eq "coerce different RGB types"
  <longrgb> (coerce <uintrgb> <bytergb>))
(test-eq "coerce integer sequence and RGB type"
  (sequence <intrgb>) (coerce (sequence <int>) (rgb <int>)))
(test-eq "coerce RGB type and integer sequence"
  (sequence <intrgb>) (coerce (rgb <int>) (sequence <int>)))
(test-eq "coerce RGB type and 2D array"
  (multiarray <intrgb> 2) (coerce (rgb <int>) (multiarray <int> 2)))
(test-eq "coerce RGB type components"
  (rgb <int>) (rgb <sint> <ubyte> <usint>))
(test-eq "coerce RGB array from array types"
  (sequence (rgb <int>)) (rgb <sint> (sequence <ubyte>) <usint>))
(test-eq "coerce 2D RGB array from array types"
  (multiarray (rgb <int>) 2) (rgb <sint> (sequence <ubyte>) (multiarray <usint> 2)))
(test-equal "'content' extracts the channels of an RGB value"
  (list 2 3 5) (content <rgb<>> (rgb 2 3 5)))
(test-equal "'content' extracts values of typed RGB value"
  (list 2 3 5) (map get (content <intrgb> (make <intrgb> #:value (rgb 2 3 5)))))
(test-eq "type matching for (rgb 2 3 5)"
  <ubytergb> (native-type (rgb 2 3 5)))
(test-skip 3)
(test-eq "type matching for (rgb 2 3.5 5)"
  (rgb <double>) (native-type (rgb 2 3.5 5)))
(test-eq "type matching for RGB value and scalar"
  (rgb <double>) (native-type (rgb 2 3 5) 1.2))
(test-eq "type matching for scalar and RGB value"
  (rgb <double>) (native-type 1.2 (rgb 2 3 5)))
(test-eq "base type of sequence applies to element type"
  (sequence <int>) (base (sequence <intrgb>)))
(test-eq "typecode of RGB value is RGB type of base type"
  <intrgb> (typecode (rgb a b c)))
(test-equal "negate RGB value"
  (rgb -2 -3 -5) (- (rgb 2 3 5)))
(test-equal "invert RGB value"
  (rgb -3 -4 -6) (~ (rgb 2 3 5)))
(test-equal "add RGB values"
  (rgb 5 7 10) (+ (rgb 1 2 3) (rgb 4 5 7)))
(test-equal "subtract RGB values"
  (rgb 5 6 6) (- (rgb 7 9 11) (rgb 2 3 5)))
(test-equal "multiply 2 with RGB value"
  (rgb 2 4 6) (* 2 (rgb 1 2 3)))
(test-equal "bitwise and for RGB values"
  (rgb 0 1 4) (& (rgb 2 3 4) 5))
(test-equal "bitwise or for RGB values"
  (rgb 7 7 5) (| (rgb 2 3 4) 5))
(test-equal "bitwise xor for RGB values"
  (rgb 7 6 1) (^ (rgb 2 3 4) 5))
(test-equal "left shift for RGB values"
  (rgb 2 4 8) (<< 1 (rgb 1 2 3)))
(test-equal "right shift for RGB values"
  (rgb 1 2 4) (>> (rgb 2 4 8) 1))
(test-equal "division for RGB values"
  (rgb 1 2 3) (/ (rgb 3 6 9) 3))
(test-assert "compare two RGB values (positive result)"
  (= (rgb 2 3 5) (rgb 2 3 5)))
(test-assert "compare two RGB values (negative result)"
  (not (= (rgb 2 3 5) (rgb 2 4 5))))
(test-assert "check two RGB values for unequal (positive result)"
  (!= (rgb 2 3 5) (rgb 2 3 6)))
(test-assert "check two RGB values for unequal (negative result)"
  (not (!= (rgb 2 3 5) (rgb 2 3 5))))
(test-equal "major number of RGB value and scalar"
  (rgb 3 3 5) (max 3 (rgb 2 3 5)))
(test-equal "minor RGB value"
  (rgb 2 2 1) (min (rgb 2 3 5) (rgb 4 2 1)))
(test-assert "RGB variable is an RGB value"
  (is-a? (var <intrgb>) <rgb>))
(let [(p (skeleton (pointer <sintrgb>)))
      (a (skeleton <sintrgb>))
      (b (skeleton <sintrgb>))]
  (test-equal "Writing RGB to memory copies red channel"
  (mov-signed (ptr <sint> (get p) 0) (get (red   a))) (caar   (code p a)))
  (test-equal "Writing RGB to memory copies green channel"
  (mov-signed (ptr <sint> (get p) 2) (get (green a))) (caadr  (code p a)))
  (test-equal "Writing RGB to memory copies blue channel"
  (mov-signed (ptr <sint> (get p) 4) (get (blue  a))) (caaddr (code p a)))
  (test-equal "Reading RGB from memory copies red channel"
  (mov-signed (get (red   a)) (ptr <sint> (get p) 0)) (caar   (code a p)))
  (test-equal "Reading RGB from memory copies green channel"
  (mov-signed (get (green a)) (ptr <sint> (get p) 2)) (caadr  (code a p)))
  (test-equal "Reading RGB from memory copies blue channel"
  (mov-signed (get (blue  a)) (ptr <sint> (get p) 4)) (caaddr (code a p)))
  (test-equal "copy red channel"
  (mov-signed (get (red   a)) (get (red   b))) (caar   (code a b)))
  (test-equal "copy green channel"
  (mov-signed (get (green a)) (get (green b))) (caadr  (code a b)))
  (test-equal "copy blue channel"
  (mov-signed (get (blue  a)) (get (blue  b))) (caaddr (code a b))))
(test-equal "compile and run identity function for RGB value"
  (rgb 3 2 5) ((jit ctx (list <intrgb>) identity) (rgb 3 2 5)))
(test-equal "compile and run identity function for RGB array"
  (list (rgb 2 3 5) (rgb 3 5 7))
  (to-list ((jit ctx (list (sequence <ubytergb>)) identity) (seq (rgb 2 3 5) (rgb 3 5 7)))))
(test-equal "extract red channel of RGB value"
  2 ((jit ctx (list <ubytergb>) red  ) (rgb 2 3 5)))
(test-equal "extract green channel of RGB value"
  3 ((jit ctx (list <ubytergb>) green) (rgb 2 3 5)))
(test-equal "extract blue channel of RGB value"
  5 ((jit ctx (list <ubytergb>) blue ) (rgb 2 3 5)))
(test-equal "compile and run code for extracting red channel of RGB array"
  '(2 3) (to-list ((jit ctx (list (sequence <ubytergb>)) red  ) (seq (rgb 2 3 5) (rgb 3 5 7)))))
(test-equal "compile and run code for extracting green channel of RGB array"
  '(3 5) (to-list ((jit ctx (list (sequence <ubytergb>)) green) (seq (rgb 2 3 5) (rgb 3 5 7)))))
(test-equal "compile and run code for extracting blue channel of RGB array"
  '(5 7) (to-list ((jit ctx (list (sequence <ubytergb>)) blue ) (seq (rgb 2 3 5) (rgb 3 5 7)))))
(test-equal "extract red channel of RGB array"
  '(2 3) (to-list (red   (seq (rgb 2 3 5) (rgb 3 5 7)))))
(test-equal "extract green channel of RGB array"
  '(3 5) (to-list (green (seq (rgb 2 3 5) (rgb 3 5 7)))))
(test-equal "extract blue channel of RGB array"
  '(5 7) (to-list (blue  (seq (rgb 2 3 5) (rgb 3 5 7)))))
(test-equal "extract red channel of scalar array"
  '(2 3 5 7) (to-list (red (seq 2 3 5 7))))
(test-equal "extract green channel of scalar array"
  '(2 3 5 7) (to-list (green (seq 2 3 5 7))))
(test-equal "extract blue channel of scalar array"
  '(2 3 5 7) (to-list (blue (seq 2 3 5 7))))
(test-equal "compile and run code to negate RGB value"
  (rgb 2 3 -5) ((jit ctx (list <bytergb>) -) (rgb -2 -3 5)))
(test-equal "compile and run code to subtract RGB values"
  (rgb 5 6 6) ((jit ctx (list <bytergb> <bytergb>) -) (rgb 7 9 11) (rgb 2 3 5)))
(test-equal "compile and run code to adding scalar to RGB value"
  (rgb 6 7 9) ((jit ctx (list <intrgb> <int>) +) (rgb 2 3 5) 4))
(test-equal "Add scalar value to RGB sequence"
  (list (rgb 2 3 5) (rgb 3 4 6)) (to-list (+ (seq (rgb 1 2 4) (rgb 2 3 5)) 1)))
(test-equal "Add scalar sequence and RGB value"
  (list (rgb 2 3 5) (rgb 3 4 6)) (to-list (+ (seq 1 2) (rgb 1 2 4))))
(test-equal "Add RGB value and scalar sequence"
  (list (rgb 2 3 5) (rgb 3 4 6)) (to-list (+ (rgb 1 2 4) (seq 1 2))))
(test-equal "Add scalar sequence and RGB value"
  (list (rgb 2 3 5) (rgb 3 4 6)) (to-list (+ (seq 1 2) (rgb 1 2 4))))
(test-equal "compile and run function building an RGB value"
  (rgb 2 3 5) ((jit ctx (list <int> <int> <int>) rgb) 2 3 5))
(test-equal "convert integer RGB to byte RGB"
  (rgb 2 3 5) ((jit ctx (list <intrgb>) (cut to-type <bytergb> <>)) (rgb 2 3 5)))
(test-equal "construct RGB value from differently typed values"
  (rgb 2 -3 256) ((jit ctx (list <ubyte> <byte> <usint>) rgb) 2 -3 256))
(let [(c (parameter <intrgb>))]
  (test-assert "Decompose RGB parameter into RGB object"
  (is-a? (decompose-value <intrgb> c) <rgb>)))
(test-assert "Compare two RGB values (positive result)"
  ((jit ctx (list <ubytergb> <ubytergb>) =) (rgb 2 3 5) (rgb 2 3 5)))
(test-assert "Compare two RGB values (negative result)"
  (not ((jit ctx (list <ubytergb> <ubytergb>) =) (rgb 2 3 5) (rgb 2 4 5))))
(test-assert "Require two RGB values to be unequal (positive result)"
  ((jit ctx (list <ubytergb> <ubytergb>) !=) (rgb 2 3 5) (rgb 2 4 5)))
(test-assert "Require two RGB values to be unequal (negative result)"
  (not ((jit ctx (list <ubytergb> <ubytergb>) !=) (rgb 2 3 5) (rgb 2 3 5))))
(test-assert "Compare  RGB value with scalar (negative result)"
  (not ((jit ctx (list <bytergb> <byte>) =) (rgb 2 3 5) 2)))
(test-assert "Compare  RGB value with scalar (positive result)"
  ((jit ctx (list <byte> <bytergb>) =) 3 (rgb 3 3 3)))
(test-equal "major value of RGB and byte sequence"
  (list (rgb 2 2 3)) (to-list ((jit ctx (list <ubytergb> (sequence <byte>)) max) (rgb 1 2 3) (seq <byte> 2))))
(test-equal "minor value of RGB and byte sequence"
  (list (rgb 1 2 2)) (to-list ((jit ctx (list <ubytergb> (sequence <byte>)) min) (rgb 1 2 3) (seq <byte> 2))))
(test-equal "compose RGB array using array for red channel"
  (list (rgb 2 7 11) (rgb 3 7 11) (rgb 5 7 11)) (to-list (rgb (seq 2 3 5) 7 11)))
(test-equal "compose RGB array using array for green channel"
  (list (rgb 2 3 11) (rgb 2 5 11) (rgb 2 7 11)) (to-list (rgb 2 (seq 3 5 7) 11)))
(test-equal "compose RGB array using array for blue channel"
  (list (rgb 2 3 5) (rgb 2 3 7) (rgb 2 3 11)) (to-list (rgb 2 3 (seq 5 7 11))))
(test-equal "compose RGB array using array for red and green channel"
  (list (rgb 2 3 5)) (to-list (rgb (seq 2) (seq 3) 5)))
(test-assert "integer RGB memory is pointerless"
  (pointerless? <intrgb>))
(test-assert "object RGB  memory is not pointerless"
  (not (pointerless? (rgb <obj>))))
(test-eqv "extract red channel of object RGB"
  2 ((jit ctx (list (rgb <obj>)) red) (rgb 2 3 5)))
(test-equal "components of RGB values are red, green, and blue"
  (list red green blue) (components <rgb<>>))
(test-equal "extract red component of object RGB sequence"
  '(2) (to-list (red (seq (rgb <obj>) (rgb 2 3 5)))))

(test-end "aiscm rgb")
