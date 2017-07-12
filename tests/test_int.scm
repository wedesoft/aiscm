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
             (aiscm element)
             (aiscm bool)
             (aiscm float)
             (aiscm int)
             (aiscm jit))


(test-begin "aiscm int")

(test-equal "equality of classes"
  (integer 32 signed) (integer 32 signed))
(test-equal "equality of predefined classes"
  <int> (integer 32 signed))
(test-equal "equality of predefined metaclass"
  <meta<int>> (class-of (integer 32 signed)))
(test-eqv "number of bits of integer class"
  64 (bits (integer 64 signed)))
(test-assert "signed-ness of signed integer class"
  (signed? (integer 64 signed)))
(test-assert "signed-ness of unsigned integer class"
  (not (signed? (integer 64 unsigned))))
(test-eqv "storage size of byte"
  1 (size-of <byte>))
(test-eqv "storage size of short integer"
  2 (size-of <sint>))
(test-eqv "storage size of unsigned integer"
  4 (size-of <uint>))
(test-eqv "storage size of long integer"
  8 (size-of <long>))
(test-eqv "storage size of short integer instance"
  2 (size-of (make <sint> #:value #x21)))
(test-equal "equal integer objects"
  (make <ubyte> #:value #x21) (make <ubyte> #:value #x21))
(test-assert "unequal integer objects"
  (not (equal? (make <ubyte> #:value #x21) (make <usint> #:value #x4321))))
(test-assert "unequal integer objects of different classes"
  (not (equal? (make <ubyte> #:value #x21) (make <usint> #:value #x21))))
(test-eqv "integer class maintains number of bits"
  128 (bits (integer 128 signed)))
(test-assert "integer class maintains signedness for signed integer"
  (signed? (integer 128 signed)))
(test-assert "integer class maintains signedness for unsigned integer"
  (not (signed? (integer 128 unsigned))))
(test-assert "signed-ness of integer instance"
  (signed? (make <int> #:value #x21)))
(test-eqv "number of bits of integer instance"
  32 (bits (make <int> #:value #x21)))
(test-equal "pack custom integer value"
  #vu8(#x01 #x02) (pack (make (integer 16 unsigned) #:value #x0201)))
(test-equal "pack unsigned byte value"
  #vu8(#xff) (pack (make <ubyte> #:value #xff)))
(test-equal "pack unsigned short integer value"
  #vu8(#x01 #x02) (pack (make <usint> #:value #x0201)))
(test-equal "pack unsigned integer value"
  #vu8(#x01 #x02 #x03 #x04) (pack (make <uint> #:value #x04030201)))
(test-equal "unpack unsigned byte value"
  (unpack <ubyte> #vu8(#xff)) (make <ubyte> #:value #xff))
(test-equal "unpack unsigned short integer value"
  (unpack <usint> #vu8(#x01 #x02)) (make <usint> #:value #x0201))
(test-equal "unpack unsigned integer value"
  (unpack <uint> #vu8(#x01 #x02 #x03 #x04)) (make <uint> #:value #x04030201))
(test-eqv "pack and unpack signed byte"
  127 (get (unpack <byte> (pack (make <byte> #:value 127)))))
(test-eqv "pack and unpack signed byte with negative number"
  -128 (get (unpack <byte> (pack (make <byte> #:value -128)))))
(test-eqv "pack and unpack signed short integer"
  32767 (get (unpack <sint> (pack (make <sint> #:value 32767)))))
(test-eqv "pack and unpack signed short integer with negative number"
  -32768 (get (unpack <sint> (pack (make <sint> #:value -32768)))))
(test-eqv "pack and unpack signed integer"
  2147483647 (get (unpack <int> (pack (make <int> #:value 2147483647)))))
(test-eqv "pack and unpack signed integer with negative number"
  -2147483648 (get (unpack <int> (pack (make <int> #:value -2147483648)))))
(test-eqv "querying element size of integer"
  1 (size (make <int> #:value 123)))
(test-assert "querying shape of integer"
  (null? (shape (make <int> #:value 123))))
(test-equal "display short integer object"
  "#<<int<16,signed>> 1234>"
  (call-with-output-string (lambda (port) (display (make <sint> #:value 1234) port))))
(test-equal "write short integer object"
  "#<<int<16,signed>> 1234>"
  (call-with-output-string (lambda (port) (write (make <sint> #:value 1234) port))))
(test-eqv "signed coercion returns largest integer type"
  32 (bits (coerce (integer 16 signed) (integer 32 signed))))
(test-eqv "signed coercion returns largest integer type"
  16 (bits (coerce (integer 16 signed) (integer 8 signed))))
(test-assert "coercion of signed-ness"
  (not (signed? (coerce (integer 8 unsigned) (integer 16 unsigned)))))
(test-assert "coercion of signed-ness"
  (signed? (coerce (integer 8 unsigned) (integer 16 signed))))
(test-assert "coercion of signed-ness"
  (signed? (coerce (integer 8 signed) (integer 16 unsigned))))
(test-assert "coercion of signed-ness"
  (signed? (coerce (integer 8 signed) (integer 16 signed))))
(test-eqv "make space for signed-unsigned operation"
  32 (bits (coerce (integer 16 signed) (integer 16 unsigned))))
(test-eqv "make space for signed-unsigned operation"
  32 (bits (coerce (integer 8 signed) (integer 16 unsigned))))
(test-eqv "check whether unsigned value fits into signed value"
  16 (bits (coerce (integer 16 signed) (integer 8 unsigned))))
(test-eqv "coercion does not allocate more than 64 bits"
  64 (bits (coerce (integer 64 signed) (integer 64 unsigned))))
(test-equal "coercion of integer and single-precision floating point"
  <float> (coerce (integer 32 signed) (floating-point single-precision)))
(test-equal "coercion of double-precision floating point and integer"
  <double> (coerce (floating-point double-precision) (integer 32 signed)))
(test-equal "foreign type of byte"
  int8 (foreign-type (integer 8 signed)))
(test-equal "foreign type of unsigned short int"
  uint16 (foreign-type (integer 16 unsigned)))
(test-equal "type matching for 255"
  <ubyte> (native-type 255))
(test-equal "type matching for 256"
  <usint> (native-type 256))
(test-equal "type matching for 65535"
  <usint> (native-type 65535))
(test-equal "type matching for 65536"
  <uint> (native-type 65536))
(test-equal "type matching for 4294967295"
  <uint> (native-type 4294967295))
(test-equal "type matching for 4294967296"
  <ulong> (native-type 4294967296))
(test-equal "type matching for 18446744073709551615"
  <ulong> (native-type 18446744073709551615))
(test-expect-fail 1)
(test-equal "type matching for 18446744073709551616"
  <double> (native-type 18446744073709551616))
(test-equal "type matching for -128"
  <byte> (native-type -128))
(test-equal "type matching for -129"
  <sint> (native-type -129))
(test-equal "type matching for -32768"
  <sint> (native-type -32768))
(test-equal "type matching for -32769"
  <int> (native-type -32769))
(test-equal "type matching for -2147483648"
  <int> (native-type -2147483648))
(test-equal "type matching for -2147483649"
  <long> (native-type -2147483649))
(test-equal "type matching for -9223372036854775808"
  <long> (native-type -9223372036854775808))
(test-expect-fail 1)
(test-equal "type matching for -9223372036854775809"
  <double> (native-type -9223372036854775809))
(test-equal "match two integers"
  <byte> (native-type 1 -1))
(test-assert "wrapping 125 creates a byte container"
  (is-a? (wrap 125) <ubyte>))
(test-eqv "wrapping 125 maintains value"
  125 (get (wrap 125)))
(test-eqv "don't wrap twice"
  125 (get (wrap (wrap 125))))
(test-expect-fail 1)
(test-equal "type matching for 1 and 1.5"
  <double> (native-type 1 1.5))
(test-eqv "get value of integer"
  123 (get (make <int> #:value 123)))
(test-eqv "set value of integer"
  123 (let [(i (make <int> #:value 0))] (set i 123) (get i)))
(test-eqv "return-value of setting integer"
  123 (set (make <int> #:value 0) 123))
(test-equal "build short integer"
  42 (build <sint> '(42)))
(test-equal "'content' returns integer values"
  '(42) (content <int<>> 42))
(test-equal "invert integer using '~'"
  -43 (~ 42))
(test-equal "bitwise and using '&'"
  2 (& 3 6))
(test-equal "bitwise and with three arguments"
  4 (& 7 14 28))
(test-equal "bitwise or using '|'"
  7 (| 3 6))
(test-equal "bitwise or with three arguments"
  7 (| 1 2 4))
(test-equal "bitwise xor using '^'"
  5 (^ 3 6))
(test-equal "bitwise xor using '^'"
  21 (^ 7 14 28))
(test-equal "shift left using '<<'"
  12 (<< 3 2))
(test-equal "shift right using '>>'"
  3 (>> 12 2))
(test-equal "shift left by one"
  6 (<< 3))
(test-equal "shift right by one"
  3 (>> 6))
(test-equal "remainder of division using '%'"
  33 (% 123 45))
(test-equal "'==' for integers"
  '(#t #f) (map == '(3 4) '(3 5)))
(test-equal "'!=' for integers"
  '(#f #t) (map != '(3 4) '(3 5)))
(test-equal "'=0' for integers"
  '(#t #f) (map =0 '(0 1)))
(test-equal "'!=0' for integers"
  '(#f #t) (map !=0 '(0 1)))
(test-eq "base type of integer is integer"
  <int> (base <int>))
(test-eqv "conjugate of integer"
  3 (conj 3))
(test-assert "integer memory is pointerless"
  (pointerless? <int>))

(test-end "aiscm int")
