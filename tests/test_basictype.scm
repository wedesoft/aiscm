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
             (srfi srfi-1)
             (oop goops)
             (system foreign)
             (rnrs bytevectors)
             (aiscm basictype))


(test-begin "aiscm basictype")

(test-group "void type"
  (test-eq "Create void type"
    <void> (class-of (make <void> #:value #f)))
  (test-eq "Content of void type"
    'content (get (make <void> #:value 'content)))
  (test-eqv "Foreign type of void is void"
    void (foreign-type <void>))
  (test-eqv "Size of void is zero"
    0 (size-of <void>))
  (test-eq "Unpacking void just returns address"
    'unspecified (unpack-value <void> 'unspecified)))

(test-group "boolean type"
  (test-eq "Create boolean type"
    <bool> (class-of (make <bool> #:value #t)))
  (test-eqv "Foreign type of boolean is boolean"
    (1+ int64) (foreign-type <bool>)))

(test-group "construct integer types"
  (for-each
    (lambda (nbits sign cls)
      (test-eq (format #f "Class for ~a-bit ~a integer should be ~a" nbits sign (class-name cls))
        cls (integer nbits sign))
      (test-eqv (format #f "~a should have ~a bits" (class-name cls) nbits)
        nbits (bits cls))
      (test-eqv (format #f "instance of ~a should have ~a bits" (class-name cls) nbits)
        nbits (bits (make cls #:value #f)))
      (test-eq (format #f "~a should be ~a" (class-name cls) sign)
        (eq? sign 'signed) (signed? cls))
      (test-eq (format #f "instance of ~a should be ~a" (class-name cls) sign)
        (eq? sign 'signed) (signed? (make cls #:value #f))))
    (list 8 8 16 16 32 32 64 64)
    (list unsigned signed unsigned signed unsigned signed unsigned signed)
    (list <ubyte> <byte> <usint> <sint> <uint> <int> <ulong> <long>)))

(test-group "integer coercions"
  (for-each
    (lambda (a b result)
      (test-eq (format #f "Type coercion of ~a and ~a should return ~a" (class-name a) (class-name b) (class-name result))
        result (coerce a b)))
    (list <sint> <int>  <sint> <uint> <int>   <usint> <byte> <uint> <ulong>)
    (list <sint> <sint> <int>  <uint> <usint> <int>   <uint> <byte> <long> )
    (list <sint> <int>  <int>  <uint> <int>   <int>   <long> <long> <long> )))

(test-group "integer values"
  (test-equal "Wrap and unwrap value"
    42 ((get (make <int> #:value (const 42))) #f))
  (test-equal "Equal values"
    (make <int> #:value 42) (make <int> #:value 42))
  (test-assert "Unequal values"
    (not (equal? (make <int> #:value 42) (make <int> #:value 43))))
  (test-assert "Differently typed values"
    (not (equal? (make <uint> #:value 42) (make <int> #:value 42)))))

(test-group "get foreign integer type"
  (for-each (lambda (type foreign)
    (test-eqv (format #f "get foreign type of ~a" (class-name type))
      foreign (foreign-type type)))
    (list <ubyte> <byte> <usint> <sint> <uint> <int> <ulong> <long>)
    (list uint8   int8   uint16  int16  uint32 int32 uint64  int64)))

(test-group "construct floating point types"
  (test-assert "single precision"
    (not (double-precision? (floating-point single-precision))))
  (test-assert "double precision"
    (double-precision? (floating-point double-precision)))
  (test-eq "compare single precision types"
    <float> (floating-point single-precision))
  (test-eq "compare double precision types"
    <double> (floating-point double-precision)))

(test-group "floating-point coercions"
  (test-equal "coerce single precision floats"
    <float> (coerce <float> <float>))
  (test-equal "coerce single and double precision float"
    <double> (coerce <float> <double>))
  (test-equal "coerce double and single precision float"
    <double> (coerce <double> <float>))
  (test-equal "coerce double precision floats"
    <double> (coerce <double> <double>)))

(test-group "floating-point values"
  (test-equal "Wrap and unwrap value"
    3.5 ((get (make <float> #:value (const 3.5))) #f))
  (test-equal "Equal values"
    (make <float> #:value 3.5) (make <float> #:value 3.5))
  (test-assert "Unequal values"
    (not (equal? (make <float> #:value 3.5) (make <float> #:value 4.5))))
  (test-assert "Differently typed values"
    (not (equal? (make <float> #:value 1.5) (make <double> #:value 1.5)))))

(test-group "get foreign floating-point type"
  (test-eqv "get foreign type of <float>"
    float (foreign-type <float>))
  (test-eqv "get foreign type of <double>"
    double (foreign-type <double>)))

(test-group "coercing floating-point and integer types"
  (test-eq "coerce single-precision floating point and integer"
    <float> (coerce <float> <int>))
  (test-eq "coerce double-precision floating point and integer"
    <double> (coerce <double> <int>))
  (test-eq "coerce integer and single-precision floating point"
    <float> (coerce <int> <float>))
  (test-eq "coerce integer and double-precision floating point"
    <double> (coerce <int> <double>)))

(test-group "complex numbers"
  (test-eq "Single-precision complex number"
    <complex<float>> (complex (floating-point single-precision)))
  (test-eq "Double-precision complex number"
    <complex<double>> (complex (floating-point double-precision)))
  (test-equal "Basic type of single-precision complex number"
    (list <float> <float>) (base <complex<float>>))
  (test-equal "Basic type of double-precision complex number"
    (list <double> <double>) (base <complex<double>>))
  (test-eq "Real component of single-precision floating-point complex number is a float"
    <float> (class-of (real-part (make <complex<float>> #:value (const '(2.5 3.25))))))
  (test-eq "Real component of double-precision floating-point complex number is a double"
    <double> (class-of (real-part (make <complex<double>> #:value (const '(2.5 3.25))))))
  (test-eqv "Get real component of complex number"
    2.5 ((get (real-part (make <complex<float>> #:value (const '(2.5 3.25))))) #f))
  (test-eq "Imaginary component of single-precision floating-point complex number is a float"
    <float> (class-of (imag-part (make <complex<float>> #:value '(2.5 3.25)))))
  (test-eq "Imaginary component of double-precision floating-point complex number is a double"
    <double> (class-of (imag-part (make <complex<double>> #:value '(2.5 3.25)))))
  (test-eqv "Get imaginary component of complex number"
    3.25 ((get (imag-part (make <complex<float>> #:value (const '(2.5 3.25))))) #f)))

(test-group "complex coercion"
  (test-eq "Coerce two complex types"
    (complex <float>) (coerce (complex <float>) (complex <float>)))
  (test-eq "Coerce single and double complex type"
    (complex <double>) (coerce (complex <float>) (complex <double>)))
  (test-eq "Coerce double and single complex type"
    (complex <double>) (coerce (complex <double>) (complex <float>)))
  (test-eq "Coerce float and complex"
    (complex <double>) (coerce <double> (complex <float>)))
  (test-eq "Coerce complex and float"
    (complex <double>) (coerce (complex <float>) <double>)))

(test-group "coerce pointers"
  (test-eq "Coerce pointer and integer"
    (pointer <int>) (coerce (pointer <int>) <long>)))

(test-eqv "get foreign type of complex type"
  int64 (foreign-type <complex<float>>))

(test-group "decompose arguments"
  (test-equal "Decompose false"
   '(0) (decompose-argument <bool> #f))
  (test-equal "Decompose true"
   '(1) (decompose-argument <bool> #t))
  (test-equal "Decompose integer"
    '(42) (decompose-argument <int> 42))
  (test-equal "Decompose floating-point number"
    '(1.25) (decompose-argument <float> 1.25))
  (test-equal "Decompose complex number"
     '(2.5 3.25) (decompose-argument <complex<float>> 2.5+3.25i))
  (test-equal "Decompose nested type"
    '(2.5 0 3.25 0) (decompose-argument (complex (complex <float>)) 2.5+3.25i))
  (test-equal "Decompose pointer"
    '(1234) (decompose-argument (pointer <byte>) (make-pointer 1234))))

(test-group "decompose result"
  (test-equal "Decompose scalar"
    '(42) (decompose-result <int> 42))
  (test-equal "Decompose complex number"
    '(2.5 3.25) (decompose-result <complex<float>> 2.5+3.25i)))

(test-group "compose value"
  (test-eq "Composing integer creates correct type"
    <sint> (class-of (car (compose-value <sint> (list (const 42))))))
  (test-eq "Return remaining values after composing integer"
    'rest (cdr (compose-value <sint> (cons (const 42) 'rest))))
  (test-equal "Composing integer uses provided value"
    42 ((get (car (compose-value <sint> (list (const 42))))) #f))
  (test-equal "Compose complex number"
    '(2.5 3.25) ((get (car (compose-value <complex<float>> (map const '(2.5 3.25))))) #f))
  (test-equal "Return remaining values after composing complex number"
    'rest (cdr (compose-value <complex<float>> (append (map const '(2.5 3.25)) 'rest))))
  (test-equal "Compose nested value"
    '((1.0 2.0) (3.0 4.0)) ((get (car (compose-value (complex (complex <float>)) (map const '(1.0 2.0 3.0 4.0))))) #f)))

(test-group "compose multiple values"
  (test-assert "Compose no values"
    (null? (compose-values '() '())))
  (test-equal "Composing an integer value creates an object of correct type"
    (list <sint>) (map class-of (compose-values (list <sint>) (list (const 42)))))
  (test-equal "Composing an integer value uses the provided value"
    42 ((get (car (compose-values (list <sint>) (list (const 42))))) #f))
  (test-equal "Composing a complex number uses two values"
    '(2.5 3.25) ((get (car (compose-values (list <complex<float>>) (list (const 2.5) (const 3.25))))) #f))
  (test-equal "Compose two integer values"
    '(5 7) (map (lambda (arg) ((get arg) #f)) (compose-values (list <int> <int>) (list (const 5) (const 7))))))

(test-group "decompose types"
  (test-equal "decompose boolean type"
    (list <bool>) (decompose-type <bool>))
  (test-equal "decompose integer type"
    (list <sint>) (decompose-type <sint>))
  (test-equal "decompose complex type"
    (list <double> <double>) (decompose-type <complex<double>>))
  (test-equal "decompose nested type"
    (make-list 4 <double>) (decompose-type (complex (complex <double>))))
  (test-equal "decompose nested mixed type"
    (make-list 4 <double>) (decompose-type (complex (complex <double>) (complex <double>)))))

(test-group "size of values"
  (test-eqv "Size of boolean"
    1 (size-of <bool>))
  (test-eqv "size of byte"
    1 (size-of <byte>))
  (test-eqv "size of integer"
    4 (size-of <int>))
  (test-eqv "size of float"
    4 (size-of <float>))
  (test-eqv "size of double"
    8 (size-of <double>))
  (test-eqv "size of single-precision complex number"
    8 (size-of <complex<float>>))
  (test-eqv "size of double-precision complex number"
    16 (size-of <complex<double>>)))

(test-group "unpack values"
  (test-eqv "unpack false"
    #f (unpack-value <bool> (pointer-address (bytevector->pointer #vu8(0)))))
  (test-eqv "unpack true"
    #t (unpack-value <bool> (pointer-address (bytevector->pointer #vu8(1)))))
  (test-eqv "unpack unsigned byte"
    200 (unpack-value <ubyte> (pointer-address (bytevector->pointer #vu8(200)))))
  (test-eqv "unpack signed byte"
    -56 (unpack-value <byte> (pointer-address (bytevector->pointer #vu8(200)))))
  (test-eqv "unpack short integer"
    8716 (unpack-value <sint> (pointer-address (bytevector->pointer #vu8(12 34)))))
  (test-eqv "unpack single-precision floating point number"
    1.375 (unpack-value <float> (pointer-address (bytevector->pointer #vu8(0 0 176 63)))))
  (test-eqv "unpack double-precision floating point number"
    1.256525 (unpack-value <double> (pointer-address (bytevector->pointer #vu8(208 179 89 245 185 26 244 63)))))
  (test-eqv "unpack single-precision complex value"
    1.375+4.0i (unpack-value <complex<float>> (pointer-address (bytevector->pointer #vu8(0 0 176 63 0 0 128 64)))))
  (test-eqv "unpack double-precision complex value"
    1+2i (unpack-value <complex<double>> (pointer-address (bytevector->pointer #vu8(0 0 0 0 0 0 240 63 0 0 0 0 0 0 0 64))))))

(test-group "type matching"
  (test-equal "type matching for #t"
    <bool> (native-type #t))
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
  (test-equal "type matching for 1.5"
    <double> (native-type 1.5))
  (test-equal "type matching for 2+3i"
    <complex<double>> (native-type 2+3i)))

(define-class <testcontainer> ()
              (testcontent #:init-keyword #:testcontent #:getter testcontent))
(define (make-testcontainer testcontent) (make <testcontainer> #:testcontent testcontent))
(define-structure testcontainer make-testcontainer (testcontent))
(define-class <testtwo> ()
              (test-a #:init-keyword #:test-a #:getter test-a)
              (test-b #:init-keyword #:test-b #:getter test-b))
(define (make-testtwo test-a test-b) (make <testtwo> #:test-a test-a #:test-b test-b))
(define-structure testtwo make-testtwo (test-a test-b))
(test-group "define composite type"
  (test-assert "'define-structure' defines an abstract composite type"
    (defined? '<testcontainer<>>))
  (test-eq "'define-structure' defines a metaclass for the abstract composite type"
    '<meta<testcontainer<>>> (class-name (class-of <testcontainer<>>)))
  (test-assert "'define-structure' defines a template type"
    (defined? 'testcontainer))
  (test-eq "Instantiate composite type"
    '<testcontainer<int<32,signed>>> (class-name (testcontainer <int>)))
  (test-eq "Super class of composite type is abstract composite type"
    '<testcontainer<>> (class-name (car (class-direct-supers (testcontainer <int>)))))
  (test-equal "'define-structure' defines method for querying base type"
    (list <int>) (base (testcontainer <int>)))
  (test-eqv "Foreign type of composite values is a pointer"
    int64 (foreign-type (testcontainer <int>)))
  (test-eq "Define method to query size of type"
    (size-of <int>) (size-of (testcontainer <int>)))
  (test-eq "Query size of type with two elements"
    (* 2 (size-of <int>)) (size-of (testtwo <int>)))
  (test-equal "Get list of member accessors"
    (list testcontent) (components <testcontainer<>>))
  (test-assert "Create member accessor for container"
    (testcontent (make (testcontainer <int>) #:value (lambda (fun) '(42)))))
  (test-assert "Create second member accessor for container"
    (test-b (make (testtwo <int>) #:value (lambda (fun) '(42 60)))))
  (test-eqv "Access content of first member"
    42 ((get (test-a (make (testtwo <int>) #:value (lambda (fun) '(42 60))))) #f))
  (test-eqv "Access content of second member"
    60 ((get (test-b (make (testtwo <int>) #:value (lambda (fun) '(42 60))))) #f))
  (test-eqv "Unpack first member of composite value"
    2 (test-a (unpack-value (testtwo <byte>) (pointer-address (bytevector->pointer #vu8(2 3))))))
  (test-eqv "Unpack second member of composite value"
    3 (test-b (unpack-value (testtwo <byte>) (pointer-address (bytevector->pointer #vu8(2 3))))))
  (test-equal "Access content of nested container"
    '(2 3) ((get (testcontent (make (testcontainer (testtwo <int>)) #:value (lambda (fun) '((2 3)))))) #f))
  (test-eq "Content type of nested container"
    (testtwo <int>) (class-of (testcontent (make (testcontainer (testtwo <int>)) #:value (lambda (fun) '((2 3)))))))
  (test-equal "Access value of nested member"
    3 ((get (test-b (testcontent (make (testcontainer (testtwo <int>)) #:value (lambda (fun) '((2 3))))))) #f)))

(define-class <testmixed> ()
              (test-a #:init-keyword #:test-a #:getter test-a)
              (test-b #:init-keyword #:test-b #:getter test-b))
(define (make-testmixed test-a test-b) (make <testmixed> #:test-a test-a #:test-b test-b))
(define-structure testmixed make-testmixed (test-a test-b))
(test-group "define composite type with multiple arguments"
  (test-eq "Instantiate mixed composite type"
    '<testmixed<int<32,signed>,int<16,signed>>> (class-name (testmixed <int> <sint>)))
  (test-equal "Get list of basic types"
    (list <int> <sint>) (base (testmixed <int> <sint>))))

(test-group "pointer types"
  (test-eq "Get target type of pointer"
    <int> (target (pointer <int>)))
  (test-eqv "Foreign type of pointer"
    int64 (foreign-type (pointer <int>)))
  (test-eqv "Size of pointer"
    8 (size-of (pointer <int>)))
  (test-equal "Decompose pointer type"
    (list <long>) (decompose-type (pointer <int>)))
  (test-equal "Decompose pointer value"
    (list 123) (decompose-argument (pointer <int>) (make-pointer 123))))

(test-group "static size lists"
  (test-eq "Instantiate static size list"
    '<tuple<int<32,signed>,3>> (class-name (tuple <int> 3)))
  (test-equal"Base of static size list"
    (list <int> <int> <int> <int> <int>) (base (tuple <int> 5)))
  (test-eq "Native type of integer list"
    (tuple <ubyte> 4) (native-type '(2 3 5 7)))
  (test-eq "Native type of floating-point list"
    (tuple <double> 4) (native-type '(2 3.5 5 7)))
  (test-eqv "Get element of tuple"
    5 (get '(2 3 5 7) 2))
  (test-eqv "Dimension of tuple"
    3 (dimension (tuple <int> 3)))
  (test-eq "Typecode of tuple"
    <int> (typecode (tuple <int> 3)))
  (test-equal "Decompose static list type"
    (list <byte> <byte> <byte>) (decompose-type (tuple <byte> 3)))
  (test-equal "Decompose static list argument"
    '(2 3 5) (decompose-argument (tuple <int> 3) '(2 3 5))))

(test-group "Multi-dimensional arrays"
  (test-eqv "Dimension of array"
    2 (dimensions (multiarray <int> 2)))
  (test-eq "Element type of array"
    <int> (typecode (multiarray <int> 2)))
  (test-equal "Shape of multi-dimensional array"
    '(3 2) (shape (make (multiarray <int> 2) #:shape '(3 2))))
  (test-eqv "Specify correct memory size to allocator"
    24 (memory (make (multiarray <int> 2) #:shape '(3 2) #:allocator identity)))
  (test-assert "Allocate memory"
    (pointer? (memory (make (multiarray <int> 2) #:shape '(3 2)))))
  (test-assert "Define base memory"
    (pointer? (memory-base (make (multiarray <int> 2) #:shape '(3 2)))))
  (test-equal "Strides of multi-dimensional array"
    '(1 2 6) (strides (make (multiarray <int> 3) #:shape '(2 3 5))))
  (test-eq "Native type of multi-dimensional array"
    (llvmarray <int> 2) (native-type (make (multiarray <int> 2) #:shape '(3 2))))
  (test-equal "Decompose multi-dimensional array type"
    (list <long> <long> <int> <int> <int> <int>) (decompose-type (llvmarray <ubyte> 2))))

(test-end "aiscm basictype")
