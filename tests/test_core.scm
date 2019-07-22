;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018, 2019 Jan Wedekind <jan@wedesoft.de>
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
             (aiscm core))


(test-begin "aiscm core")

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
    3.25 ((get (imag-part (make <complex<float>> #:value (const '(2.5 3.25))))) #f))
  (test-eqv "Complex conjugate"
    2-3i (conj 2+3i))
  (test-equal "Complex conjugate of array"
    '(2-3i 5-7i) (to-list (conj (arr 2+3i 5+7i))))
  (test-equal "Complex conjugate of real array"
    '(2 3 5) (to-list (conj (arr 2 3 5))))
  (test-assert "Equal complex numbers"
    ((jit (list <complex<float>> <complex<float>>) eq) 2+3i 2+3i))
  (test-assert "Unequal complex numbers"
    (not ((jit (list <complex<float>> <complex<float>>) eq) 2+3i 2+5i)))
  (test-equal "Complex absolute value"
    '(5.0) (to-list (abs (arr 3+4i)))))

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

(test-group "coerce objects"
  (test-eq "Coerce object and integer"
    <obj> (coerce <obj> <int>))
  (test-eq "Coerce integer and objecgt"
    <obj> (coerce <int> <obj>)))

(test-group "coerce pointers"
  (test-eq "Coerce pointer and integer"
    (pointer <int>) (coerce (pointer <int>) <long>)))

(test-eqv "get foreign type of complex type"
  uint64 (foreign-type <complex<float>>))

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
  (test-eq "type matching for #t"
    <bool> (native-type #t))
  (test-eq "type matching for #t and #f"
    <bool> (native-type #t #f))
  (test-eq "Native type for #t and 0"
    <obj> (native-type #t 0))
  (test-eq "type matching for 255"
    <ubyte> (native-type 255))
  (test-eq "type matching for 256"
    <usint> (native-type 256))
  (test-eq "type matching for 65535"
    <usint> (native-type 65535))
  (test-eq "type matching for 65536"
    <uint> (native-type 65536))
  (test-eq "type matching for 4294967295"
    <uint> (native-type 4294967295))
  (test-eq "type matching for 4294967296"
    <ulong> (native-type 4294967296))
  (test-eq "type matching for 18446744073709551615"
    <ulong> (native-type 18446744073709551615))
  (test-eq "type matching for -128"
    <byte> (native-type -128))
  (test-eq "type matching for 1 and -1"
    <byte> (native-type 1 -1))
  (test-eq "type matching for -129"
    <sint> (native-type -129))
  (test-eq "type matching for -32768"
    <sint> (native-type -32768))
  (test-eq "type matching for -32769"
    <int> (native-type -32769))
  (test-eq "type matching for -2147483648"
    <int> (native-type -2147483648))
  (test-eq "type matching for -2147483649"
    <long> (native-type -2147483649))
  (test-eq "type matching for -9223372036854775808"
    <long> (native-type -9223372036854775808))
  (test-eq "type matching for very large positive integer"
    <double> (native-type (ash 1 64)))
  (test-eq "type matching for very large negative integer"
    <double> (native-type (- (ash 1 64))))
  (test-eq "type matching for 1.5"
    <double> (native-type 1.5))
  (test-eq "type matching for 1 and 1.5"
    <double> (native-type 1 1.5))
  (test-eq "type matching for 2+3i"
    <complex<double>> (native-type 2+3i))
  (test-eq "type matching for 1 and 2+3i"
    <complex<double>> (native-type 1 2+3i))
  (test-eq "Type matching for 'abc"
    <obj> (native-type 'abc)))

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
    uint64 (foreign-type (testcontainer <int>)))
  (test-eq "Define method to query size of type"
    (size-of <int>) (size-of (testcontainer <int>)))
  (test-eq "Query size of type with two elements"
    (* 2 (size-of <int>)) (size-of (testtwo <int>)))
  (test-eq "Two instantiation constructors"
    (testtwo <int>) (testtwo <int> <int>))
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
    uint64 (foreign-type (pointer <int>)))
  (test-eqv "Size of pointer"
    8 (size-of (pointer <int>)))
  (test-equal "Decompose pointer type"
    (list <ulong>) (decompose-type (pointer <int>)))
  (test-equal "Decompose pointer value"
    (list 123) (decompose-argument (pointer <int>) (make-pointer 123))))

(test-group "static size lists"
  (test-eq "Instantiate static size list"
    '<llvmlist<int<32,signed>,3>> (class-name (llvmlist <int> 3)))
  (test-equal"Base of static size list"
    (list <int> <int> <int> <int> <int>) (base (llvmlist <int> 5)))
  (test-eq "Native type of integer list"
    (llvmlist <ubyte> 4) (native-type '(2 3 5 7)))
  (test-eq "Native type of floating-point list"
    (llvmlist <double> 4) (native-type '(2 3.5 5 7)))
  (test-eqv "Get element of llvmlist"
    5 (get '(2 3 5 7) 2))
  (test-eqv "Dimension of llvmlist"
    3 (dimension (llvmlist <int> 3)))
  (test-eq "Typecode of llvmlist"
    <int> (typecode (llvmlist <int> 3)))
  (test-equal "Decompose static list type"
    (list <byte> <byte> <byte>) (decompose-type (llvmlist <byte> 3)))
  (test-equal "Decompose static list argument"
    '(2 3 5) (decompose-argument (llvmlist <int> 3) '(2 3 5))))

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
    '(24 8 4) (strides (make (multiarray <int> 3) #:shape '(5 3 2))))
  (test-eq "Native type of multi-dimensional array"
    (llvmarray <int> 2) (native-type (make (multiarray <int> 2) #:shape '(3 2))))
  (test-equal "Decompose multi-dimensional array type"
    (list <ulong> <ulong> <int> <int> <int> <int>) (decompose-type (llvmarray <ubyte> 2)))
  (test-equal "Size of array"
    (* 4 32 20) (size-of (make (multiarray <int> 2) #:shape '(32 20)))))

(test-group "module"
  (test-equal "Create LLVM instance"
    <llvm> (class-of (make-llvm-module)))
  (test-assert "Destroy LLVM instance"
    (unspecified? (destroy (make-llvm-module))))
  (test-assert "LLVM module slot defined"
    (slot-ref (make-llvm-module) 'llvm-module)))

(test-group "constant values"
  (test-eqv "Get type of 1-bit boolean value"
    llvm-bool (get-type ((make-constant llvm-bool #t) #f)))
  (for-each
    (lambda (type bits)
      (test-eqv (format #f "Get type of ~a-bit integer value" bits)
        type (get-type ((make-constant type 42) #f))))
    (list llvm-int8 llvm-int16 llvm-int32 llvm-int64)
    '(8 16 32 64))
  (for-each
    (lambda (unsigned-type signed-type bits)
      (test-eqv (format #f "Type of ~a-bit value ignores signed-ness" bits)
        signed-type (get-type ((make-constant unsigned-type 42) #f))))
    (list llvm-uint8 llvm-uint16 llvm-uint32 llvm-uint64)
    (list llvm-int8 llvm-int16 llvm-int32 llvm-int64)
    '(8 16 32 64))
  (test-equal "Get type of double-precision floating point value"
    llvm-double (get-type ((make-constant llvm-double (exp 1)) #f)))
  (test-equal "Get type of single-precision floating point value"
    llvm-float (get-type ((make-constant llvm-float (exp 1)) #f)))
  (test-equal "Memory address is 64 bit"
    llvm-int64 (get-type ((make-constant-pointer (make-pointer 1234)) #f))))

(test-group "memoization"
  (test-eqv "implements function"
    42 ((memoize () 42)))
  (test-eqv "accept parameters"
    5 ((memoize (x y) (+ x y)) 2 3))
  (test-eqv "multiple statements"
    21 ((memoize (x) (set! x (1+ x)) x) 20))
  (let* [(x 20)
         (f (memoize () (set! x (1+ x)) x))]
    (test-eqv "cache return value"
      21 (begin (f) (f)))))

(test-group "functions"
  (test-equal "Create LLVM function"
    <llvm-function> (class-of (let [(mod (make-llvm-module))] (make-function mod llvm-void "function"))))
  (let [(mod (make-llvm-module))]
    (test-equal "Keep LLVM instance alive"
      mod (slot-ref (make-function mod llvm-void "function") 'module)))
  (test-assert "Compile, verify, and run empty function"
    (unspecified?
      (let* [(mod  (make-llvm-module))
             (fun  (make-function mod llvm-void "empty"))]
        ((function-ret) fun)
        (llvm-compile mod)
        ((llvm-func mod fun)))))
  (test-assert "Dump module containing a function"
    (unspecified?
      (let* [(mod  (make-llvm-module))
             (fun  (make-function mod llvm-void "empty"))]
        ((function-ret) fun)
        (llvm-dump mod))))
  (test-error "Throw error if module is not valid"
    'misc-error
    (let* [(mod  (make-llvm-module))
           (fun  (make-function mod llvm-void "incomplete"))]
      (llvm-compile llvm)))
  (test-error "Throw error when attempting to compile twice"
    'misc-error
    (let* [(mod  (make-llvm-module))
           (fun  (make-function mod llvm-void "empty"))]
      ((function-ret) fun)
      (llvm-compile llvm)
      (llvm-compile llvm)))
  (for-each
    (lambda (type sign bits value)
      (test-equal (format #f "Compile and run function returning a ~a ~a-bit integer" sign bits)
        value
        (let* [(mod (make-llvm-module))
               (fun (make-function mod type "constant_int"))]
          ((function-ret (make-constant type value)) fun)
          (llvm-compile mod)
          ((llvm-func mod fun)))))
    (list llvm-int8 llvm-int16 llvm-int32 llvm-int64 llvm-uint8 llvm-uint16 llvm-uint32 llvm-uint64)
    (append (make-list 4 "signed") (make-list 4 "unsigned"))
    '(8 16 32 64 8 16 32 64)
    '(-128 -32768 -2147483648 -9223372036854775808 255 65535 4294967295 18446744073709551615))
  (for-each
    (lambda (type precision)
      (test-equal (format #f "Compile and run function returning a ~a-precision floating point number" precision)
         0.5
         (let* [(mod  (make-llvm-module))
                (fun  (make-function mod type "constant_double"))]
           ((function-ret (make-constant type 0.5)) fun)
           (llvm-compile mod)
           ((llvm-func mod fun)))))
    (list llvm-float llvm-double)
    (list "single" "double")))

(test-group "pointers"
  (for-each (lambda (value type name)
    (test-equal (format #f "Read ~a value from memory" name)
      value
      (let* [(data #vu8(2 3 5 7))
             (mod  (make-llvm-module))
             (fun  (make-function mod type "read_mem"))]
        ((function-ret (llvm-fetch type (make-constant-pointer (bytevector->pointer data)))) fun)
        (llvm-compile mod)
        ((llvm-func mod fun)))))
    '(2 770)
    (list llvm-int8 llvm-int16)
    '("byte" "short integer"))
  (for-each (lambda (data value type name)
    (test-equal (format #f "Write ~a to memory" name)
      #vu8(2 3 5 7)
      (let* [(mod  (make-llvm-module))
             (fun  (make-function mod llvm-void "write_mem"))]
        ((llvm-store type (make-constant type value) (make-constant-pointer (bytevector->pointer data))) fun)
        ((function-ret) fun)
        (llvm-compile mod)
        ((llvm-func mod fun))
        data)))
    (list #vu8(0 3 5 7) #vu8(0 0 5 7))
    '(2 770)
    (list llvm-int8 llvm-int16)
    '("byte" "short integer")))

(test-group "method arguments"
  (test-assert "Declare a function which accepts arguments"
    (let [(mod (make-llvm-module))]
      (make-function mod llvm-int32 "with_arg" llvm-int32)))
  (test-assert "Call a function accepting an argument"
    (let* [(mod  (make-llvm-module))
           (fun  (make-function mod llvm-void "accept_arg" llvm-int32))]
      ((function-ret) fun)
      (llvm-compile mod)
      ((llvm-func mod fun) 42)))
  (for-each (lambda (value type name)
    (test-equal (format #f "Compile, verify, and run ~a identity function" name)
      value
      (let* [(mod  (make-llvm-module))
             (fun  (make-function mod type "identity" type))]
        ((function-ret (function-param 0)) fun)
        (llvm-compile mod)
        ((llvm-func mod fun) value))))
    '(42 0.5)
    (list llvm-int32 llvm-double)
    '("integer" "floating-point"))
  (for-each (lambda (value type name)
    (test-equal (format #f "Compile, verify, and run function returning second ~a argument" name)
      value
      (let* [(mod  (make-llvm-module))
             (fun  (make-function mod type "second" llvm-int32 type))]
        ((function-ret (function-param 1)) fun)
        (llvm-compile mod)
        ((llvm-func mod fun) -1 value))))
    '(42 0.5)
    (list llvm-int32 llvm-double)
    '("integer" "floating-point")))

(test-group "unary operation"
  (for-each (lambda (value op result type)
    (test-equal (format #f "(~a ~a) should be ~a" (procedure-name op) value result)
      result
      (let* [(mod (make-llvm-module))
             (fun (make-function mod type "op" type))]
        ((function-ret (op (function-param 0))) fun)
        (llvm-compile mod)
        ((llvm-func mod fun) value))))
    '(42 42 2.5)
    (list llvm-not llvm-neg llvm-fneg)
    '(213 -42 -2.5)
    (list llvm-uint8 llvm-int32 llvm-double)))

(test-group "binary expressions"
  (for-each (lambda (value-a value-b op result type)
    (test-equal (format #f "(~a ~a ~a) should be ~a" (procedure-name op) value-a value-b result)
      result
      (let* [(mod (make-llvm-module))
             (fun (make-function mod type "add" type type))]
        ((function-ret (op (function-param 0) (function-param 1))) fun)
        (llvm-compile mod)
        ((llvm-func mod fun) value-a value-b))))
    '(2 100  5 6  6 2.5  5.75 2.5  2.6 3 12 -12 13 -13 5.5 3 3)
    '(3  30  7 2 -2 3.25 3.25 3.25 0.4 2  2   2  5   5   3 5 5)
    (list llvm-add llvm-sub llvm-mul llvm-udiv llvm-sdiv llvm-fadd llvm-fsub llvm-fmul llvm-fdiv
          llvm-shl llvm-lshr llvm-ashr llvm-urem llvm-srem llvm-frem llvm-and llvm-or)
    '(5  70 35 3 -3 5.75 2.5  8.125 6.5 12 3 -3  3  -3 2.5 1 7)
    (list llvm-int32 llvm-int32 llvm-int32 llvm-int32 llvm-int32 llvm-double llvm-double llvm-double llvm-double
          llvm-int32 llvm-int32 llvm-int32 llvm-int32 llvm-int32 llvm-double llvm-int32 llvm-int32)))

(test-group "convenience wrapper"
  (test-assert "Define empty function using convenience wrapper"
    (unspecified? ((llvm-wrap '() (const (cons llvm-void (function-ret)))))))
  (test-eqv "Define constant function using convenience wrapper"
    42
    ((llvm-wrap '() (const (cons llvm-int32 (function-ret (make-constant llvm-int32 42)))))))
  (test-eqv "Define integer identity function using convenience wrapper"
    42
    ((llvm-wrap (list llvm-int32) (lambda (value) (cons llvm-int32 (function-ret value)))) 42))
  (test-eqv "Use byte values instead of booleans"
    1
    ((llvm-wrap (list llvm-bool) (lambda (value) (cons llvm-bool (function-ret value)))) 1))
  (test-eqv "Define negating function using convenience wrapper"
    -42
    ((llvm-wrap (list llvm-int32) (lambda (value) (cons llvm-int32 (function-ret (llvm-neg value))))) 42))
  (test-eqv "Define addition function using convenience wrapper"
    36
    ((llvm-wrap (list llvm-int32 llvm-int32)
                (lambda (value-a value-b) (cons llvm-int32 (function-ret (llvm-add value-a value-b))))) 21 15))
  (test-equal "Define function with side-effect but no return value"
    #vu8(42)
    (let* [(data    #vu8(0))
           (pointer (make-constant-pointer (bytevector->pointer data)))]
      ((llvm-wrap (list llvm-int8)
                  (lambda (value)
                    (cons llvm-void (lambda (fun) ((llvm-store llvm-int8 value pointer) fun) ((function-ret) fun))))) 42)
      data))
  (test-eqv "Pass pointer argument"
    42
    (let* [(data    #vu8(42))
           (pointer (pointer-address (bytevector->pointer data)))]
      ((llvm-wrap (list llvm-int64)
                  (lambda (value) (cons llvm-int8 (function-ret (llvm-fetch llvm-int8 value))))) pointer))))

(test-group "integer type conversions"
  (test-equal "Zero-extend integer"
    254
    (let* [(data #vu8(254))
           (pointer (pointer-address (bytevector->pointer data)))]
      ((llvm-wrap (list int64)
                  (lambda (value) (cons uint32 (function-ret (llvm-zext uint32 (llvm-fetch uint8 value)))))) pointer)))
  (test-equal "Sign-extend integer"
    -2
    (let* [(data #vu8(254))
           (pointer (pointer-address (bytevector->pointer data)))]
      ((llvm-wrap (list llvm-int64)
                  (lambda (value) (cons llvm-int32 (function-ret (llvm-sext llvm-int32 (llvm-fetch llvm-int8 value)))))) pointer)))
  (test-equal "Truncate integer"
    #xcd ((llvm-wrap (list llvm-uint16)
                     (lambda (value) (cons llvm-uint8 (function-ret (llvm-trunc llvm-int8 value))))) #xabcd)))

(test-group "expression basics"
  (test-equal "unary minus invokes llvm negation"
    -42
    (let* [(mod (make-llvm-module))
           (fun (make-function mod llvm-int32 "op" llvm-int32))]
      ((function-ret (get (- (make <int> #:value (function-param 0))))) fun)
      (llvm-compile mod)
      ((llvm-func mod fun) 42)))
  (for-each (lambda (type)
    (test-equal (format #f "unary minus should preserve ~a type" type)
      type (class-of (- (make type #:value (function-param 0))))))
    (list <ubyte> <byte> <usint> <sint> <uint> <int> <ulong> <long>)))

(test-group "integer type conversion"
  (test-equal "trivial conversion"
    42 ((jit (list <int>) (cut to-type <int> <>)) 42))
  (test-equal "truncating integer conversion"
    #xcd ((jit (list <uint>) (cut to-type <ubyte> <>)) #xabcd))
  (test-equal "zero-extending integer conversion"
    200 ((jit (list <ubyte>) (lambda (value) (to-type <int> (to-type <ubyte> value)))) 200))
  (test-equal "zero-extending integer conversion"
    200 ((jit (list <ubyte>) (lambda (value) (to-type <int> (to-type <ubyte> value)))) 200))
  (test-equal "sign-extending integer conversion"
    -42 ((jit (list <byte>) (lambda (value) (to-type <int> (to-type <byte> value)))) -42))
  (test-equal "convert boolean to unsigned byte"
    '(0 1) (to-list (to-type <ubyte> (arr #f #t))))
  (test-equal "convert unsigned byte to boolean"
    '(#f #t #t) (to-list (to-type <bool> (arr 0 1 2)))))

(test-group "type inference"
  (test-equal "identity function with integer"
    42 ((jit (list <int>) identity) 42))
  (test-equal "identity function returning true"
    #t ((jit (list <bool>) identity) #t))
  (test-equal "identity function returning false"
    #f ((jit (list <bool>) identity) #f))
  (test-equal "compact integer negation"
    -42 ((jit (list <int>) -) 42))
  (test-equal "sum of two integers"
    5 ((jit (list <int> <int>) +) 2 3))
  (test-equal "sum of unsigned byte and byte"
    382 ((jit (list <ubyte> <byte>) +) 255 127)))

(test-group "floating-point type conversions"
  (test-equal "convert single-precision to double-precision float"
    3.5 ((llvm-wrap (list llvm-float) (lambda (value) (cons llvm-double (function-ret (llvm-fp-cast llvm-double value))))) 3.5))
  (test-equal "convert double-precision to single-precision float"
    3.5 ((llvm-wrap (list llvm-double) (lambda (value) (cons llvm-float (function-ret (llvm-fp-cast llvm-float value))))) 3.5)))

(test-group "convert floating-point to integer"
  (test-equal "convert floating-point to signed integer "
    -42 ((llvm-wrap (list llvm-float) (lambda (value) (cons llvm-int32 (function-ret (llvm-fp-to-si llvm-int32 value))))) -42.0))
  (test-equal "convert floating-point to unsigned integer "
    200 ((llvm-wrap (list llvm-float) (lambda (value) (cons llvm-uint8 (function-ret (llvm-fp-to-ui llvm-uint8 value))))) 200.0)))

(test-group "convert integer to floating-point"
  (test-equal "convert signed integer to floating-point"
    -42.0 ((llvm-wrap (list llvm-int32) (lambda (value) (cons llvm-float (function-ret (llvm-si-to-fp llvm-float value))))) -42))
  (test-equal "convert unsigned integer to floating-point"
    200.0 ((llvm-wrap (list llvm-uint8) (lambda (value) (cons llvm-double (function-ret (llvm-ui-to-fp llvm-double value))))) 200)))

(test-group "floating point type conversions"
  (test-equal "returns requested type"
    <double> (class-of (to-type <double> (typed-constant <float> 42.5))))
  (test-equal "perform conversion"
    42.5 ((jit (list <float>) (cut to-type <double> <>)) 42.5)))

(test-group "convert between integer and floating-point"
  (test-equal "signed byte to float"
    -42.0 ((jit (list <byte>) (cut to-type <float> <>)) -42))
  (test-equal "unsigned byte to float"
    200.0 ((jit (list <ubyte>) (cut to-type <float> <>)) 200))
  (test-equal "convert float to signed integer"
    -42 ((jit (list <int>) (lambda (value) (to-type <int> (to-type <double> value)))) -42))
  (test-equal "convert float to unsigned int"
    200 ((jit (list <uint>) (lambda (value) (to-type <uint> (to-type <double> value)))) 200)))

(test-group "integer unary expressions"
  (for-each (lambda (op result)
    (test-equal (format #f "(~a 42) should be ~a" (procedure-name op) result)
      result ((jit (list <int>) op) 42)))
    (list ~ -)
    '(-43 -42)))

(test-group "floating-point unary expression"
  (test-equal "(- 42.5) single-precision should be -42.5"
    -42.5 ((jit (list <float>) -) 42.5))
  (test-equal "(- 42.5) double-precision should be -42.5"
    -42.5 ((jit (list <double>) -) 42.5)))

(test-group "integer binary expressions"
  (for-each (lambda (op a b result)
    (test-eqv (format #f "(~a 2 3) should be ~a" (procedure-name op) result)
      result ((jit (list <int> <int>) op) a b)))
    (list +  - * <<  % & | ^)
    '(    2  2 2  2 13 3 3 5)
    '(    3  3 3  3  5 5 5 4)
    '(    5 -1 6 16  3 1 7 1))
  (test-eqv "Unsigned integer division"
    127 ((jit (list <ubyte> <ubyte>) /) 254 2))
  (test-eqv "Signed integer division"
    -64 ((jit (list <byte> <byte>) /) -128 2))
  (test-eqv "Right shift unsigned integer"
    127 ((jit (list <ubyte> <ubyte>) >>) 254 1))
  (test-eqv "Right shift signed integer"
    -1 ((jit (list <byte> <byte>) >>) -2 1))
  (test-eqv "Unsigned integer remander"
    4 ((jit (list <ubyte> <ubyte>) %) 254 50)))

(test-group "floating-point binary expression"
  (for-each (lambda (value-a value-b op result)
    (test-equal (format #f "(~a ~a ~a) should be ~a" (procedure-name op) value-a value-b result)
      result ((jit (list (if (integer? value-a) <int> <float>)
                         (if (integer? value-b) <int> <float>))
                          op) value-a value-b)))
    '(2.5  2.5 2    3.75 2   2.5 1.5 2    1.25 2.25 2.5 3   5.5)
    '(3.75 3   3.75 2.5  1.5 1   2.5 1.25 2    0.5  2   1.5 2.5)
    (list + + + - - - * * * / / / %)
    '(6.25 5.5 5.75 1.25 0.5 1.5 3.75 2.5 2.5 4.5 1.25 2.0 0.5)))

(test-group "constant conversions"
  (test-eqv "add integer constant to value"
    5 ((jit (list <int>) (lambda (x) (+ x 3))) 2))
  (test-eqv "add value to integer constant"
    5 ((jit (list <int>) (lambda (x) (+ 2 x))) 3))
  (test-eqv "subtract integer constant from value"
    2 ((jit (list <int>) (lambda (x) (- x 3))) 5))
  (test-eqv "subtract value from integer constant"
    3 ((jit (list <int>) (lambda (x) (- 5 x))) 2))
  (test-eqv "add floating-point constant to value"
    5.5 ((jit (list <float>) (lambda (x) (+ x 3))) 2.5))
  (test-eqv "add floating-point constant to integer value"
    5.5 ((jit (list <int>) (lambda (x) (+ x 3.5))) 2))
  (test-eqv "add complex number to integer value"
    7+3i ((jit (list <int>) (lambda (x) (+ x 2+3i))) 5))
  (test-eqv "add complex number to complex value"
    7+10i ((jit (list <complex<float>>) (lambda (x) (+ x 2+3i))) 5+7i)))

(test-group "composite types"
  (test-eqv "return real part of complex number"
    2.5 ((jit (list <complex<float>>) real-part) 2.5+3.25i))
  (test-eqv "return imaginary part of complex number"
    3.25 ((jit (list <complex<float>>) imag-part) 2.5+3.25i))
  (test-eqv "complex single-precision identity"
    2+3i ((jit (list <complex<float>>) identity) 2+3i))
  (test-eqv "complex double-precision identity"
    2+3i ((jit (list <complex<double>>) identity) 2+3i))
  (test-eqv "compose single-precision complex number"
    2+3i ((jit (list <float> <float>) complex) 2 3))
  (test-eqv "compose double-precision complex number"
    2+3i ((jit (list <double> <double>) complex) 2 3))
  (test-eqv "compose complex number from different precision numbers"
    2+3i ((jit (list <float> <double>) complex) 2 3))
  (test-eqv "complex negation"
    -2-3i ((jit (list <complex<double>>) -) 2+3i))
  (test-eqv "complex plus"
    7+10i ((jit (list <complex<double>> <complex<double>>) +) 2+3i 5+7i))
  (test-eqv "add scalar to complex value"
    7+3i ((jit (list <complex<double>> <int>) +) 2+3i 5))
  (test-eqv "add complex value to scalar"
    5+5i ((jit (list <float> <complex<float>>) +) 2 3+5i))
  (test-eqv "complex minus"
    -3-4i ((jit (list <complex<double>> <complex<double>>) -) 2+3i 5+7i))
  (test-eqv "subtract scalar from complex value"
    -3+3i ((jit (list <complex<double>> <int>) -) 2+3i 5))
  (test-eqv "subtract complex value from scalar"
    -1-5i ((jit (list <float> <complex<float>>) -) 2 3+5i))
  (test-eqv "real part of real number"
    5.5 ((jit (list <float>) real-part) 5.5))
  (test-eqv "imaginary part of real number"
    0.0 ((jit (list <float>) imag-part) 5.5))
  (test-eqv "convert float to complex"
    5.0+0.0i ((jit (list <float>) (cut to-type <complex<float>> <>)) 5))
  (test-eqv "change precision of complex number"
    2.0+3.0i ((jit (list (complex <float>)) (cut to-type <complex<double>> <>)) 2+3i))
  (test-eqv "complex multiplication"
    -11+29i ((jit (list (complex <float>) (complex <float>)) *) 2+3i 5+7i))
  (test-eqv "complex-scalar multiplication"
    10+15i ((jit (list (complex <float>) <float>) *) 2+3i 5))
  (test-eqv "scalar-complex multiplication"
    10+15i ((jit (list <float> (complex <float>)) *) 5 2+3i))
  (test-eqv "complex division"
    2+3i ((jit (list (complex <float>) (complex <float>)) /) -11+29i 5+7i))
  (test-eqv "complex-scalar division"
    1+2i ((jit (list (complex <float>) <float>) /) 5+10i 5))
  (test-eqv "scalar-complex division"
    4-6i ((jit (list <float> (complex <float>)) /) 26 2+3i)))

(test-group "method calls"
  (test-eqv "call libc's fabsf method"
    1.25
    ((llvm-wrap (list llvm-float)
                (lambda args (cons llvm-float (function-ret (llvm-call llvm-float "fabsf" (list llvm-float) args)))))
     -1.25))
  (test-eqv "call libc's atan2 method"
    0.0
    ((llvm-wrap (list llvm-double llvm-double)
                (lambda args (cons llvm-double (function-ret (llvm-call llvm-double "atan2" (list llvm-double llvm-double) args)))))
     0.0 1.0))
  (test-eqv "typed call of fabsf method"
    1.25
    ((jit (list <float>) (lambda (arg) (typed-call <float> "fabsf" (list <float>) (list arg)))) -1.25)))

(test-group "pointers"
  (test-equal "Pointer identity function"
    (make-pointer 123) ((jit (list (pointer <int>)) identity) (make-pointer 123)))
  (test-eqv "Fetch pointer value"
    42 ((jit (list (pointer <byte>)) fetch) (bytevector->pointer #vu8(42 63))))
  (test-equal "Typecast when writing to pointer target"
    #vu8(42 3 5 7)
    (let [(data #vu8(2 3 5 7))]
      ((jit (list (pointer <byte>) <int>) (lambda (ptr value) (store ptr value)))
       (bytevector->pointer data) 42)
      data)))

(test-group "typed constants"
  (test-eq "Type of constant should be of specified type"
    <sint> (class-of (typed-constant <sint> 42)))
  (test-equal "Use corresponding foreign type"
    int16 (get-type ((get (typed-constant <sint> 42)) #f)))
  (test-equal "Pointer constant is 64-bit"
    int64 (get-type ((get (typed-pointer <int> (make-pointer 1234))) #f)))
  (test-equal "Pointer type"
    (pointer <int>) (class-of (typed-pointer <int> (make-pointer 1234))))
  (test-equal "boolean constant"
    #t ((jit '() (const (typed-constant <bool> #t)))))
  (test-equal "integer constant"
    42 ((jit '() (const (typed-constant <int> 42)))))
  (test-equal "floating-point constant"
    2.5 ((jit '() (const (typed-constant <double> 2.5)))))
  (test-equal "complex constant"
    2+3i ((jit '() (const (typed-constant <complex<float>> 2+3i))))))

(test-group "typed store/fetch"
  (let* [(data #vu8(0 3 5 7))
         (ptr  (typed-pointer <byte> (bytevector->pointer data)))]
    (test-equal "write byte to memory"
      #vu8(2 3 5 7)
      (begin ((jit (list <byte>) (lambda (value) (store ptr value))) 2) data)))
  (let* [(data #vu8(0 3 5 7))
         (ptr  (typed-pointer <byte> (bytevector->pointer data)))]
    (test-assert "storing a value returns no value"
      (unspecified? ((jit (list <byte>) (lambda (value) (store ptr value))) 2))))
  (let* [(data #vu8(2 3 5 7))
         (ptr  (typed-pointer <byte> (bytevector->pointer data)))]
    (test-eqv "read byte from memory"
      2 ((jit '() (lambda () (fetch ptr))))))
  (let* [(data #vu8(1 2 3 4 5 6 7 8 9 10))
         (ptr  (typed-pointer (complex <float>) (bytevector->pointer data)))]
    (test-equal "write complex number to memory"
      #vu8(0 0 0 64 0 0 64 64 9 10)
      (begin ((jit (list <complex<float>>) (lambda (value) (store ptr value))) 2+3i) data)))
  (let* [(data #vu8(0 0 0 64 0 0 64 64))
         (ptr  (typed-pointer (complex <float>) (bytevector->pointer data)))]
    (test-eqv "read complex number from memory"
      2+3i ((jit '() (lambda () (fetch ptr)))))))

(test-group "instruction sequence"
  (test-eqv "test single instruction"
    42 ((jit (list <int>) (lambda (value) (llvm-begin value))) 42))
  (test-eqv "test two instructions"
    42 ((jit (list <int>) (lambda (value) (llvm-begin value value))) 42))
  (let* [(data #vu8(0 0 0 0))
         (ptr  (typed-pointer <int> (bytevector->pointer data)))]
    (test-eqv "ensure both instructions are executed"
      42 ((jit (list <int>) (lambda (value) (llvm-begin (store ptr value) (fetch ptr)))) 42))))

(define-uniform-constructor testcontainer)
(test-group "operations for custom composite type"
  (test-assert "compile identity operation for composite type"
    (jit (list (testcontainer <int>)) identity))
  (test-eqv "run identity operation for composite type"
    42 (testcontent ((jit (list (testcontainer <int>)) identity) (make-testcontainer 42))))
  (test-assert "compile constructor of test container"
    (jit (list <int>) testcontainer))
  (test-equal "Uniform container performs type coercion"
    (complex <float>) (class-of (complex (typed-constant <int> 2) (typed-constant <float> 3)))))

(define-mixed-constructor testmixed)
(test-group "operations for mixed composite type"
  (let* [(data #vu8(3 5 7))
         (ptr  (typed-pointer (testmixed <sint> <byte>) (bytevector->pointer data)))]
    (test-eqv "read first value of mixed variable"
      1283 (test-a ((jit '() (lambda () (fetch ptr))))))
    (test-eqv "read second value of mixed variable"
      7 (test-b ((jit '() (lambda () (fetch ptr)))))))
    (let* [(data #vu8(1 2 3 4))
           (ptr  (typed-pointer (testmixed <sint> <byte>) (bytevector->pointer data)))]
      (test-equal "write composite value to memory"
        #vu8(3 5 7 4)
        (begin
          ((jit (list (testmixed <sint> <byte>)) (lambda (value) (store ptr value)))
           (make-testmixed 1283 7))
          data)))
  (test-equal "Mixed constructor preserves types of components"
    (testmixed <sint> <byte>) (class-of (testmixed (typed-constant <sint> 2) (typed-constant <byte> 3)))))

(test-group "comparison"
  (test-eqv "unsigned less-than"
    #t ((jit (list <ubyte> <ubyte>) lt) 120 140))
  (test-eqv "not unsigned less-than"
    #f ((jit (list <ubyte> <ubyte>) lt) 120 120))
  (test-eqv "signed less-than"
    #t ((jit (list <byte> <ubyte>) lt) -100 100))
  (test-eqv "not signed less-than"
    #f ((jit (list <ubyte> <byte>) lt) 100 -100))
  (test-eqv "type coercion for signed less-than"
    #t ((jit (list <byte> <ubyte>) lt) -1 255))
  (test-eqv "unsigned less-than or equal"
    #t ((jit (list <ubyte> <ubyte>) le) 120 120))
  (test-eqv "not unsigned less-than or equal"
    #f ((jit (list <ubyte> <ubyte>) le) 120 119))
  (test-eqv "signed less-than or equal"
    #t ((jit (list <byte> <ubyte>) le) -100 100))
  (test-eqv "not signed less-than or equal"
    #f ((jit (list <ubyte> <byte>) le) 100 -100))
  (test-eqv "unsigned greater-than"
    #t ((jit (list <ubyte> <ubyte>) gt) 140 120))
  (test-eqv "not unsigned greater-than"
    #f ((jit (list <ubyte> <ubyte>) gt) 120 120))
  (test-eqv "signed greater-than"
    #t ((jit (list <ubyte> <byte>) gt) 100 -100))
  (test-eqv "not signed greater-than"
    #f ((jit (list <byte> <ubyte>) gt) -100 100))
  (test-eqv "unsigned greater-than or equal"
    #t ((jit (list <ubyte> <ubyte>) ge) 120 120))
  (test-eqv "not unsigned greater-than or equal"
    #f ((jit (list <ubyte> <ubyte>) ge) 119 120))
  (test-eqv "signed greater-than or equal"
    #t ((jit (list <ubyte> <byte>) ge) 100 -100))
  (test-eqv "not signed greater-than or equal"
    #f ((jit (list <byte> <ubyte>) ge) -100 100))
  (test-eqv "not equal integers"
    #f ((jit (list <byte> <byte>) eq) 120 100))
  (test-eqv "equal integers"
    #t ((jit (list <byte> <byte>) eq) 100 100))
  (test-eqv "unequal integers"
    #t ((jit (list <byte> <byte>) ne) 120 100))
  (test-eqv "not unequal integers"
    #f ((jit (list <byte> <byte>) ne) 100 100))
  (test-eqv "floating-point less-than"
    #t ((jit (list <float> <float>) lt) 2.5 3.0))
  (test-eqv "floating-point not less-than"
    #f ((jit (list <float> <float>) lt) 3.0 3.0))
  (test-eqv "compare floating-point and integer"
    #t ((jit (list <float> <int>) lt) 2.5 3))
  (test-eqv "compare integer and floating-point"
    #t ((jit (list <int> <float>) lt) 2 3))
  (test-eqv "floating-point less-than or equal"
    #t ((jit (list <float> <float>) le) 3.0 3.0))
  (test-eqv "floating-point not less-than or equal"
    #f ((jit (list <float> <float>) le) 3.5 3.0))
  (test-eqv "floating-point greater-than"
    #t ((jit (list <float> <float>) gt) 2.5 2.0))
  (test-eqv "not floating-point greater-than"
    #f ((jit (list <float> <float>) gt) 3.5 3.5))
  (test-eqv "floating-point greater-than or equal"
    #t ((jit (list <float> <float>) ge) 2.5 2.5))
  (test-eqv "not floating-point greater-than or equal"
    #f ((jit (list <float> <float>) ge) 1.0 1.5))
  (test-eqv "not equal floating-point numbers"
    #f ((jit (list <float> <float>) eq) 1.0 1.5))
  (test-eqv "equal floating-point numbers"
    #t ((jit (list <float> <float>) eq) 1.5 1.5))
  (test-eqv "unequal floating-point numbers"
    #t ((jit (list <float> <float>) ne) 1.0 1.5))
  (test-eqv "not unequal floating-point numbers"
    #f ((jit (list <float> <float>) ne) 1.5 1.5))
  (test-eqv "pointer comparison"
    #t ((jit (list (pointer <byte>) (pointer <byte>)) gt) (make-pointer 2) (make-pointer 1))))

(test-group "local variables"
  (test-eqv "Typed let without any definitions"
    42 ((jit (list <int>) (lambda (value) (jit-let [] value))) 42))
  (test-eqv "Typed let with a definition"
    42 ((jit '() (lambda () (jit-let [(a (typed-constant <int> 42))] a)))))
  (test-eqv "Typed let with two definitions"
    5 ((jit '() (lambda () (jit-let [(a (typed-constant <int> 2)) (b (typed-constant <int> 3))] (+ a b)))))))

(test-group "basic blocks and branch instructions"
  (test-equal "branch instruction"
    42
    ((jit (list <int>)
                 (lambda (x)
                   (let [(block (make-basic-block "block"))]
                     (llvm-begin
                       (build-branch block)
                       (position-builder-at-end block)
                       x))))
     42)))

(test-group "conditional statement"
  (test-equal "implement absolute value"
    '(3 5)
    (map (jit (list <int>) (lambda (x) (where (lt x 0) (- x) x))) '(3 -5)))
  (test-eqv "scalar coercion"
    3.5 ((jit (list <int> <float>) (lambda (x y) (where (lt x y) x y))) 5 3.5))
  (test-eqv "conditional with composite value"
    2.0+3.0i
    ((jit (list (complex <float>) (complex <float>)) (lambda (x y) (where (lt (real-part x) (real-part y)) x y)))
     2+3i 5+7i))
  (test-eqv "conditional with constant in second place"
    3 ((jit (list <int>) (lambda (x) (where (gt x 3) 3 x))) 5))
  (test-eqv "conditional with constant in third place"
    3 ((jit (list <int>) (lambda (x) (where (gt x 3) x 3))) 1)))

(test-group "alloca and phi values"
  (test-eqv "Allocate a variable on the stack"
    42
    ((jit (list <int>)
                 (lambda (x)
                   (jit-let [(ptr (typed-alloca <int>))]
                     (store ptr x)
                     (fetch ptr)))) 42)))

(test-group "Static size list"
  (test-eqv "Access element of list"
    5 ((jit (list (llvmlist <int> 4)) (cut get <> 2)) '(2 3 5 7)))
  (test-equal "Identity function for list"
    '(2 3 5) ((jit (list (llvmlist <int> 3)) identity) '(2 3 5)))
  (test-equal "Create static size list"
    '(2 3 5) ((jit (list <int> <int> <int>) llvmlist) 2 3 5))
  (test-eqv "First element of list"
    2 ((jit (list (llvmlist <int> 3)) llvm-car) '(2 3 5)))
  (test-equal "Get tail of list"
    '(3 5) ((jit (list (llvmlist <int> 3)) llvm-cdr) '(2 3 5))) )

(test-group "Multi-dimensional array"
  (let [(m0 (make (multiarray <byte> 0) #:shape '() #:memory (bytevector->pointer #vu8(42))))
        (m1 (make (multiarray <byte> 1) #:shape '(3) #:memory (bytevector->pointer #vu8(2 3 5))))
        (s1 (make (multiarray <sint> 1) #:shape '(3) #:memory (bytevector->pointer #vu8(0 0 0 0 2 3))))
        (m2 (make (multiarray <byte> 2) #:shape '(2 3) #:memory (bytevector->pointer #vu8(2 3 5 7 11 13))))
        (s2 (make (multiarray <sint> 2) #:shape '(2 3) #:memory (bytevector->pointer #vu8(2 3 5 7 11 13 5 7 11 13 17 19))))]
    (test-equal "Identity function preserves shape"
      '(2 3 5) (shape ((jit (list (llvmarray <int> 3)) identity) (make (multiarray <int> 3) #:shape '(2 3 5)))))
    (test-equal "Shape can be queried in compiled code"
      '(6 4) ((jit (list (llvmarray <int> 2)) shape) (make (multiarray <int> 2) #:shape '(6 4))))
    (test-eqv "Get element of 0D byte array"
      42 (get m0))
    (test-eqv "Get first element of 1D byte array"
      2 (get m1 0))
    (test-eqv "Get third element of 1D byte array"
      5 (get m1 2))
    (test-eqv "Get third element of 1D short integer array"
      (+ 2 (* 3 256)) (get s1 2))
    (test-equal "Get slice of array"
      '(7 11 13) (to-list (get m2 1)))
    (test-equal "Get range of elements from 1D array"
      '(3 5) (to-list (get (arr 2 3 5 7) '(1 . 3))))
    (test-equal "Set element of 1D array"
      '(2 7 5) (let [(m  (arr 2 3 5))] (set m 1 7) (to-list m)))
    (test-equal "Set element of 2D array"
      '((1 7 3) (4 5 6)) (let [(m  (arr (1 2 3) (4 5 6)))] (set m 1 0 7) (to-list m)))
    (test-equal "Set range of elements of 1D array"
      '(2 11 13 7) (let [(m  (arr 2 3 5 7))] (set m '(1 . 3) (arr 11 13)) (to-list m)))
    (test-equal "Set range of elements of 1D array to same value"
      '(2 11 11 7) (let [(m  (arr 2 3 5 7))] (set m '(1 . 3) 11) (to-list m)))
    (test-equal "Getting range rolls dimensions of array"
      '(4 3 1) (shape (get (make (multiarray <int> 3) #:shape '(5 4 3)) '(0 . 1))))
    (test-equal "Build multiarray with correct memory"
      (memory m2)
      (memory ((jit (list (pointer <byte>) (pointer <byte> ) (llvmlist <int> 2) (llvmlist <int> 2)) llvmarray)
               (memory m2) (memory-base m2) (shape m2) (strides m2))))
    (test-equal "Build multiarray with correct base memory"
      (memory-base m2)
      (memory-base ((jit (list (pointer <byte>) (pointer <byte> ) (llvmlist <int> 2) (llvmlist <int> 2)) llvmarray)
                    (memory m2) (memory-base m2) (shape m2) (strides m2))))
    (test-equal "Build multiarray with correct shape"
      (shape m2)
      (shape ((jit (list (pointer <byte>) (pointer <byte> ) (llvmlist <int> 2) (llvmlist <int> 2)) llvmarray)
              (memory m2) (memory-base m2) (shape m2) (strides m2))))
    (test-equal "Build multiarray with correct strides"
      (strides m2)
      (strides ((jit (list (pointer <byte>) (pointer <byte> ) (llvmlist <int> 2) (llvmlist <int> 2)) llvmarray)
                (memory m2) (memory-base m2) (shape m2) (strides m2))))
    (test-equal "Slice of array has adjusted memory pointer"
      (make-pointer (+ (pointer-address (memory m2)) 3)) (memory (get m2 1)))
    (test-equal "Memory pointer of slice is adjusted according to type size"
      (make-pointer (+ (pointer-address (memory s2)) 6)) (memory (get s2 1)))
    (test-equal "Slice keeps memory base pointer to prevent premature garbage collection"
      (memory-base m2) (memory-base (get m2 1)))
    (test-equal "Shape of slice"
      '(3) (shape (get m2 1)))
    (test-equal "Strides of slice"
      '(1) (strides (get m2 1)))
    (test-equal "2D element access"
      13 (get m2 2 1))
    (test-equal "Convert 1D array to list"
      '(2 3 5) (to-list m1))
    (test-equal "Convert 2D array to list"
      '((2 3 5) (7 11 13)) (to-list m2))
    (test-equal "Write empty 1D array"
      "#<multiarray<int<8,signed>,1>>:\n()"
      (call-with-output-string (cut write (make (multiarray <byte> 1) #:shape '(0)) <>)))
    (test-equal "Write 0D array"
      "#<multiarray<int<8,signed>,0>>:\n42"
      (call-with-output-string (cut write m0 <>)))
    (test-equal "Write large 1D array"
      "#<multiarray<int<8,unsigned>,1>>:\n(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ...)"
      (call-with-output-string (cut write (to-array (make-list 80 1)) <>)))
    (test-equal "Write large 2D array"
      "#<multiarray<int<8,unsigned>,2>>:\n((1)\n (1)\n (1)\n (1)\n (1)\n (1)\n (1)\n (1)\n (1)\n (1)\n ..."
      (call-with-output-string (cut write (to-array (make-list 80 '(1))) <>)))
    (test-equal "Shape of 1D list"
      '(3) (shape '(2 3 5)))
    (test-equal "Shape of 2D list"
      '(2 3) (shape '((2 3 5) (3 5 7))))
    (test-eq "Convert list to array"
      (multiarray <ubyte> 1) (class-of (to-array '(2 3 5))))
    (test-equal "1D array conversion round trip"
      '(2 3 5) (to-list (to-array '(2 3 5))))
    (test-equal "2D array conversion round trip"
      '((2 3 5) (3 5 7)) (to-list (to-array '((2 3 5) (3 5 7)))))
    (test-equal "Conversion round trip with specified type"
      '(2 3 5) (to-list (to-array <int> '(2 3 5))))
    (test-eq "Use specified type"
      <int> (typecode (to-array <int> '(2 3 5))))
    (test-equal "Print 1D array"
      "#<multiarray<int<8,signed>,1>>:\n(2 3 5)"
      (call-with-output-string (cut write m1 <>)))
    (test-equal "Print 2D array"
      "#<multiarray<int<8,signed>,2>>:\n((2 3 5)\n (7 11 13))"
      (call-with-output-string (cut write m2 <>)))
    (test-equal "Shape of rolled array"
      '(3 4 2) (shape (roll (make (multiarray <int> 3) #:shape '(2 3 4)))))
    (test-equal "Strides of rolled array"
      '(16 4 48) (strides (roll (make (multiarray <int> 3) #:shape '(2 3 4)))))
    (test-equal "Shape of unrolled array"
      '(4 2 3) (shape (unroll (make (multiarray <int> 3) #:shape '(2 3 4)))))
    (test-equal "Strides of unrolled array"
      '(4 48 16) (strides (unroll (make (multiarray <int> 3) #:shape '(2 3 4)))))
    (test-equal "Crop 1D array"
      '(1 2 3) (to-list (crop 3 (arr 1 2 3 4))))
    (test-equal "Crop 2D array"
      '((1) (4)) (to-list (crop '(2 1) (arr (1 2 3) (4 5 6)))))
    (test-equal "Shape of cropped 3D array"
      '(1 2 5) (shape (crop '(1 2) (make (multiarray <int> 3) #:shape '(3 4 5)))))
    (test-equal "Dump elements from 1D array"
      '(3 4 5) (to-list (dump 2 (arr 1 2 3 4 5))))
    (test-equal "Dump elements from 2D array"
      '((2 3) (5 6)) (to-list (dump '(0 1) (arr (1 2 3) (4 5 6)))))
    (test-equal "Shape of 3D array after dumping elements"
      '(2 2 5) (shape (dump '(1 2) (make (multiarray <int> 3) #:shape '(3 4 5)))))))

(test-group "array operations"
  (test-equal "Unary minus on 1D array"
    '(-1 2 -3) (to-list (- (to-array '(1 -2 3)))))
  (test-equal "Unary operation uses strides"
    '(-1 2 -3) (to-list (- (make (multiarray <byte> 1)
                                 #:shape '(3)
                                 #:strides '(2)
                                 #:memory (bytevector->pointer #vu8(1 0 254 0 3 0))))))
  (test-equal "Unary operation computes strides"
    '(3 1) (strides (- (to-array '((1 2 3) (4 5 6))))))
  (test-equal "Unary minus on 2D array"
    '((-1 2 -3) (4 -5 6)) (to-list (- (to-array '((1 -2 3) (-4 5 -6))))))
  (test-equal "Unary negation on 1D array"
    '(254 253 252) (to-list (~ (to-array '(1 2 3)))))
  (test-equal "Result of 1D binary plus"
    '(7 10) (to-list (+ (to-array '(2 3)) (to-array '(5 7)))))
  (test-equal "Result of 2D binary plus"
    '((7 10)) (to-list (+ (to-array '((2 3))) (to-array '((5 7))))))
  (test-equal "Result of array plus scalar"
    '(7 8) (to-list (+ (to-array '(2 3)) 5)))
  (test-equal "Result of scalar plus array"
    '(7 8) (to-list (+ 5 (to-array '(2 3)))))
  (test-equal "Result of 2D plus 1D array"
    '((4 5 7) (10 14 16)) (to-list (+ (to-array '((2 3 5) (7 11 13))) (to-array '(2 3)))))
  (test-equal "Result of 1D plus 2D array"
    '((4 5 7) (10 14 16)) (to-list (+ (to-array '(2 3)) (to-array '((2 3 5) (7 11 13))))))
  (test-equal "Subtract 1D arrays"
    '(3 4) (to-list (- (to-array '(5 7)) (to-array '(2 3)))))
  (test-equal "Multiply 1D array"
    '(10 21) (to-list (* (to-array '(2 3)) (to-array '(5 7)))))
  (test-equal "Divide 1D array"
    '(2 3) (to-list (/ (to-array '(10 21)) (to-array '(5 7)))))
  (test-equal "Modulo 1D array"
    '(2 1) (to-list (% (to-array '(10 21)) 4)))
  (test-equal "Modulo single value"
    2 (% 10 4))
  (test-equal "Left-shift array"
    '(20 14) (to-list (<< (arr 5 7) (arr 2 1))))
  (test-equal "Right-shift array"
    '(5 7) (to-list (>> (arr 20 14) (arr 2 1))))
  (test-equal "Elementwise and of array"
    '(0 1) (to-list (& (arr 2 3) 5)))
  (test-equal "Elementwise and of array"
    '(7 7) (to-list (| (arr 2 3) 5)))
  (test-equal "Default strides"
    '(24 8 4) (default-strides <int> '(5 3 2)))
  (let [(m (to-array '(2 3 5)))
        (n (make (multiarray <ubyte> 1) #:shape '(3) #:strides '(2) #:memory (bytevector->pointer #vu8(2 2 3 3 5 5))))]
    (test-equal "Duplication preserves content"
      '(2 3 5) (to-list (duplicate m)))
    (test-assert "Duplication creates new memory"
      (not (eq? (memory m) (memory (duplicate m)))))
    (test-eq "Pass-through compact array"
      (memory m) (memory (ensure-default-strides m)))
    (test-assert "Copy array if not compact"
      (not (eq? (memory n) (memory (ensure-default-strides n))))))
  (test-equal "Compose complex array"
    (list 2+5i 3+5i) (to-list (complex (to-array '(2 3)) 5)))
  (test-equal "Compose RGB array"
    (list (rgb 2 3 5) (rgb 2 3 7)) (to-list (rgb 2 3 (to-array '(5 7)))))
  (test-equal "Extract real part of complex array"
    '(2.0 5.0) (to-list (real-part (to-array '(2+3i 5+7i)))))
  (test-equal "Extract imaginary part of complex array"
    '(3.0 7.0) (to-list (imag-part (to-array '(2+3i 5+7i)))))
  (test-equal "Extract real part of real array"
    '(2 3) (to-list (real-part (arr 2 3))))
  (test-equal "Extract imaginary of real array"
    '(0 0) (to-list (imag-part (arr 2 3))))
  (test-equal "Extract red part of of RGB array"
    '(2 7) (to-list (red (to-array (list (rgb 2 3 5) (rgb 7 11 13))))))
  (test-equal "Extract green part of RGB array"
    '(3 11) (to-list (green (to-array (list (rgb 2 3 5) (rgb 7 11 13))))))
  (test-equal "Extract blue part of RGB array"
    '(5 13) (to-list (blue (to-array (list (rgb 2 3 5) (rgb 7 11 13))))))
  (test-equal "Equal comparison"
    '(#f #t #f) (to-list (eq (arr 2 3 5) (arr 5 3 2))))
  (test-equal "Not-equal comparison"
    '(#t #f #t) (to-list (ne (arr 2 3 5) (arr 5 3 2))))
  (test-equal "Greater-than comparison"
    '(#f #f #t) (to-list (gt (arr 2 3 5) (arr 5 3 2))))
  (test-equal "Greater-equal comparison"
    '(#f #t #t) (to-list (ge (arr 2 3 5) (arr 5 3 2))))
  (test-equal "Equal for complex numbers"
    '(#f #t) (to-list (eq (arr 3+2i 3+4i) 3+4i)))
  (test-equal "not-equal for complex numbers"
    '(#t #f) (to-list (ne (arr 3+2i 3+4i) 3+4i)))
  (test-equal "Lower-than comparison"
    '(#t #f #f) (to-list (lt (arr 2 3 5) (arr 5 3 2))))
  (test-equal "Lower-equal comparison"
    '(#t #t #f) (to-list (le (arr 2 3 5) (arr 5 3 2))))
  (test-equal "Element-wise selection"
    '(5 3) (to-list (where (arr #f #t) (arr 2 3) (arr 5 7))))
  (test-equal "Element-wise selection with real and complex number"
    '(2+3i 0.0+0.0i) (to-list (where (arr #t #f) (arr 2+3i 5+7i) 0)))
  (test-equal "Element-wise selection with gray and RGB"
    (list (rgb 2 3 5) (rgb 0 0 0)) (to-list (where (arr #t #f) (to-array (list (rgb 2 3 5) (rgb 3 5 7))) 0)))
  (test-equal "Element-wise minor value"
    '(2 3 2) (to-list (minor (arr 2 3 5) (arr 5 3 2))))
  (test-equal "Element-wise major value"
    '(5 3 5) (to-list (major (arr 2 3 5) (arr 5 3 2))))
  (test-equal "minor value of RGB values (compiled)"
    (rgb 2 3 2) ((jit (list (rgb <int>) (rgb <int>)) minor) (rgb 2 3 5) (rgb 5 3 2)))
  (test-equal "major value of RGB values (compiled)"
    (rgb 5 3 5) ((jit (list (rgb <int>) (rgb <int>)) major) (rgb 2 3 5) (rgb 5 3 2)))
  (test-equal "minor value of RGB values"
    (rgb 2 3 2)  (minor (rgb 2 3 5) (rgb 5 3 2)))
  (test-equal "major value of RGB values"
    (rgb 5 3 5)  (major (rgb 2 3 5) (rgb 5 3 2))))

(test-group "Array macro"
  (test-equal "Define array using macro"
    '((1 2) (3 4)) (to-list (arr (1 2) (3 4))))
  (test-equal "Define typed array using macro"
    '((1 2) (3 4)) (to-list (arr <int> (1 2) (3 4))))
  (test-eq "Set typecode of array using macro"
    <int> (typecode (arr <int> (1 2) (3 4)))))

(test-group "RGB values"
  (test-equal "display RGB value"
    "(rgb 1 2 3)" (call-with-output-string (lambda (port) (write (rgb 1 2 3) port))))
  (test-eqv "extract red channel of RGB value"
    2 (red (rgb 2 3 5)))
  (test-eqv "extract green channel of RGB value"
    3 (green (rgb 2 3 5)))
  (test-eqv "extract blue channel of RGB value"
    5 (blue (rgb 2 3 5)))
  (test-eqv "extract red channel of gray value"
    2 (red 2))
  (test-eqv "extract green channel of gray value"
    3 (green 3))
  (test-eqv "extract blue channel of gray value"
    5 (blue 5))
  (test-eqv "compiled RGB type"
    3 ((jit (list (rgb <int>)) green) (rgb 2 3 5)))
  (test-equal "compiled RGB constructor"
    (rgb 2 3 5) ((jit (list <int> <int> <int>) rgb) 2 3 5))
  (test-eq "unsigned byte RGB"
    (rgb <ubyte>) <rgb<ubyte>>)
  (test-eq "floating-point RGB"
    (rgb <float>) <rgb<float>>)
  (test-eq "double-precision floating-point RGB"
    (rgb <double>) <rgb<double>>)
  (test-eq "native type of integer RGB"
    (rgb <ubyte>) (native-type (rgb 2 3 5)))
  (test-eq "native type of integer RGB"
    (rgb <double>) (native-type (rgb 2 3 5) 2.1))
  (test-eq "coerce two RGB values"
    (rgb <sint>) (coerce (rgb <ubyte>) (rgb <byte>)))
  (test-eq "coerce RGB value and scalar"
    (rgb <sint>) (coerce (rgb <ubyte>) <byte>))
  (test-eq "coerce scalar and RGB value"
    (rgb <sint>) (coerce <byte> (rgb <ubyte>)))
  (test-equal "RGB unary minus"
    (rgb -2 3 -5) ((jit (list (rgb <byte>)) -) (rgb 2 -3 5)))
  (test-equal "RGB unary not"
    (rgb 255 254 253) ((jit (list (rgb <ubyte>)) ~) (rgb 0 1 2)))
  (test-equal "RGB binary plus"
    (rgb 9 14 18) ((jit (list (rgb <byte>) (rgb <byte>)) +) (rgb 2 3 5) (rgb 7 11 13)))
  (test-equal "RGB-scalar binary plus"
    (rgb 9 10 12) ((jit (list <byte> (rgb <byte>)) +) 7 (rgb 2 3 5) 7))
  (test-equal "scalar-RGB binary plus"
    (rgb 9 10 12) ((jit (list (rgb <byte>) <byte>) +) (rgb 2 3 5) 7))
  (test-equal "RGB binary minus"
    (rgb 5 8 8) ((jit (list (rgb <byte>) (rgb <byte>)) -) (rgb 7 11 13) (rgb 2 3 5)))
  (test-equal "RGB binary multiplication"
    (rgb 14 33 65) ((jit (list (rgb <byte>) (rgb <byte>)) *) (rgb 2 3 5) (rgb 7 11 13)))
  (test-equal "RGB binary division"
    (rgb 2 3 5) ((jit (list (rgb <byte>) (rgb <byte>)) /) (rgb 14 33 65) (rgb 7 11 13)))
  (test-equal "RGB modulo (compiled)"
    (rgb 2 3 1)  ((jit (list (rgb <byte>) <byte>) %) (rgb 2 3 5) 4))
  (test-equal "modulo on RGB value"
    (rgb 2 3 1) (% (rgb 2 3 5) 4))
  (test-assert "RGB equal values"
    ((jit (list (rgb <ubyte>) (rgb <ubyte>)) eq) (rgb 2 3 5) (rgb 2 3 5)))
  (test-assert "RGB unequal values"
    (not ((jit (list (rgb <ubyte>) (rgb <ubyte>)) eq) (rgb 2 3 5) (rgb 2 3 7))))
  (test-assert "test numerical equal RGB values"
    (= (rgb 1 2 3)  (rgb 1.0 2.0 3.0)))
  (test-assert "test numerical unequal RGB values"
    (not (= (rgb 1 2 3) (rgb 1 2 4))))
  (test-assert "equal RGB and real value"
    (= (rgb 3 3 3) 3.0))
  (test-assert "unequal RGB and real value"
    (not (= (rgb 3 3 4) 3.0)))
  (test-assert "equal real and RGB value"
    (= 3.0 (rgb 3 3 3)))
  (test-assert "unequal real and RGB value"
    (not (= 3.0 (rgb 3 3 4))))
  (test-equal "RGB plus"
    (rgb 2 5 7) (+ (rgb 1 2 3) (rgb 1 3 4)))
  (test-equal "RGB minus"
    (rgb 1 2 3) (- (rgb 2 3 5) (rgb 1 1 2)))
  (test-equal "RGB division"
    (rgb 2 3 5) (/ (rgb 4 6 10) 2)))

(test-group "type conversions"
  (test-equal "convert to float"
    '(2.0 3.0 5.0) (to-list (to-type <float> (arr 2 3 5))))
  (test-equal "convert bool to bool"
    '(#f #t) (to-list (to-type <bool> (arr #f #t)))))

(test-group "boolean operations"
  (test-equal "element-wise unary negation"
    '(#t #f) (to-list (! (arr #f #t))))
  (test-equal "element-wise boolean and"
    '(#f #f #f #t) (to-list (&& (arr #f #t #f #t) (arr #f #f #t #t))))
  (test-equal "element-wise boolean or"
    '(#f #t #t #t) (to-list (|| (arr #f #t #f #t) (arr #f #f #t #t))))
  (test-assert "'and' for three operands"
    ((jit (list <bool> <bool> <bool>) &&) #t #t #t))
  (test-assert "'or' for three operands"
    ((jit (list <bool> <bool> <bool>) ||) #f #f #t)))

(test-group "Reducing operations"
  (test-eqv "sum of 1D array"
    10 (sum (arr 2 3 5)))
  (test-eqv "use correct strides when summing"
    10 (sum (get (roll (arr (2 10) (3 20) (5 40))) 0)))
  (test-eqv "sum of 2D array"
    41 (sum (arr (2 3 5) (7 11 13))))
  (test-eqv "upcast result type for bytes"
    256 (sum (arr 255 1)))
  (test-eqv "upcast result type for integers"
    (ash 1 32) (sum (to-array (list (ash 1 31) (ash 1 31)))))
  (test-eqv "sum of floating point values"
    3.75 (sum (arr 1.5 2.25)))
  (test-equal "upcast result type for RGB values"
    (rgb 256 256 256) (sum (to-array (list (rgb 255 255 255) (rgb 1 1 1)))))
  (test-eqv "product of 1D array"
    30 (product (arr 2 3 5)))
  (test-eqv "Minimum of array"
    2 (min (arr 2 3 5)))
  (test-eqv "Maximum of array"
    5 (max (arr 2 3 5)))
  (test-equal "Allocate RGB value on stack"
    <rgb> (class-of ((jit '() (lambda () (fetch (typed-alloca (rgb <int>))))))))
  (test-equal "Sum of RGB array"
    (rgb 9 14 18) (sum (to-array (list (rgb 2 3 5) (rgb 7 11 13))))))

(test-group "convolution"
  (test-equal "trivial convolution"
    '(2 3 5) (to-list (convolve (arr 2 3 5) (arr 1))))
  (test-equal "convolution multiplication using simple kernel"
    '(4 6 10) (to-list (convolve (arr 2 3 5) (arr 2))))
  (test-equal "convolution with impulse kernel"
    '(2 3 5) (to-list (convolve (arr 2 3 5) (arr 0 1 0))))
  (test-equal "Test even sized filter"
    '(2 3 5) (to-list (convolve (arr 2 3 5) (arr 0 1))))
  (test-equal "convolution of integer with short integer"
    '(2 3 5) (to-list (convolve (arr <int> 2 3 5) (arr <sint> 0 1 0))))
  (test-equal "coerce short integer and integer to integer"
    <int> (typecode (convolve (arr <sint> 2 3 5) (arr <int> 0 1 0))))
  (test-equal "RGB-scalar convolution"
    (list (rgb 4 6 10)) (to-list (convolve (to-array (list (rgb 2 3 5))) (arr 2))))
  (test-equal "convolution with 3-element shift-left kernel"
    '(2 3 0) (to-list (convolve (crop 3 (dump 1 (arr 0 1 2 3 0))) (arr 1 0 0))))
  (test-equal "do not read over lower array boundary"
    '(0 0 0) (to-list (convolve (crop 3 (dump 1 (arr 1 0 0 0 0))) (arr 1 2 4))))
  (test-equal "convolution with 3-element shift-right kernel"
    '(0 1 2) (to-list (convolve (crop 3 (dump 1 (arr 0 1 2 3 0))) (arr 0 0 1))))
  (test-equal "do not read over upper array boundary"
    '(0 0 0) (to-list (convolve (crop 3 (dump 1 (arr 0 0 0 0 1))) (arr 1 2 4))))
  (test-equal "trivial 2D convolution"
    '((2 3 5) (7 11 13)) (to-list (convolve (arr (2 3 5) (7 11 13)) (arr (1)))))
  (test-equal "test impulse in first dimension"
    '((2 3) (5 7) (11 13)) (to-list (convolve (arr (2 3) (5 7) (11 13)) (arr (0) (1) (0)))))
  (test-equal "test impulse in last dimension"
    '((2 3 5) (7 11 13)) (to-list (convolve (arr (2 3 5) (7 11 13)) (arr (0 1 0)))))
  (test-assert "large convolution"
    (convolve (fill <int> '(1000000) 1) (arr 1))))

(test-group "dilation/erosion"
  (test-equal "trivial dilation"
    '(2 3 5) (to-list (dilate (arr 2 3 5) (arr 255))))
  (test-equal "non-trivial dilation"
    '(3 5 5) (to-list (dilate (arr 2 3 5) (arr 255 255 255))))
  (test-equal "structuring element of dilation"
    '(2 3 5) (to-list (dilate (arr 2 3 5) (arr 0 255 0))))
  (test-equal "dilation with kernel size"
    '(3 5 5) (to-list (dilate (arr 2 3 5) 3)))
  (test-equal "trivial erosion"
    '(2 3 5) (to-list (erode (arr 2 3 5) (arr 0))))
  (test-equal "non-trivial erosion"
    '(2 2 3) (to-list (erode (arr 2 3 5) (arr 0 0 0))))
  (test-equal "structuring element of erosion"
    '(2 3 5) (to-list (erode (arr 2 3 5) (arr 255 0 255))))
  (test-equal "erosion with kernel size"
    '(2 2 3) (to-list (erode (arr 2 3 5) 3))))

(test-group "fill"
  (test-eq "Specify typecode of result"
    <sint> (typecode (fill <sint> '(3) 42)))
  (test-eq "Dimensions of result"
    3 (dimensions (fill <sint> '(2 3 5) 42)))
  (test-equal "Fill with value"
    '(42 42 42) (to-list (fill <sint> '(3) 42))))

(test-group "absolute value"
  (test-equal "Absolute value of integer"
    '(3 3) (map (jit (list <int>) abs) '(3 -3)))
  (test-equal "Absolute value of short integer"
    '(3 3) (map (jit (list <sint>) abs) '(3 -3)))
  (test-equal "Absolute value of single-precision floating point number"
    '(3.5 3.5) (map (jit (list <float>) abs) '(3.5 -3.5)))
  (test-equal "Absolute value of double-precision floating point number"
    '(3.5 3.5) (map (jit (list <double>) abs) '(3.5 -3.5)))
  (test-equal "Element-wise absolute value"
    '(2 3 5) (to-list (abs (arr 2 -3 5)))))

(test-group "square root and exponentiation"
  (test-equal "Square root of single-precision floating point numbers"
    '(2.0 0.5) (to-list (sqrt (arr <float> 4.0 0.25))))
  (test-equal "Square root of double-precision floating point numbers"
    '(2.0 0.5) (to-list (sqrt (arr <double> 4.0 0.25))))
  (test-equal "Square root of integer values"
    '(2.0 3.0) (to-list (sqrt (arr <int> 4 9))))
  (test-equal "Power function of single precision float"
    '(2.0 9.0) (to-list (pow (arr <float> 2.0 3.0) (arr <float> 1.0 2.0))))
  (test-equal "Power function of double precision float"
    '(2.0 9.0) (to-list (pow (arr <double> 2.0 3.0) (arr <double> 1.0 2.0)))))

(test-group "trigonometric functions"
  (test-approximate "Sinus of single-precision floating point number"
    1.0 (car (to-list (sin (arr <float> 1.5705)))) 0.001)
  (test-approximate "Sinus of double-precision floating point number"
    1.0 (car (to-list (sin (arr <double> 1.5705)))) 0.001)
  (test-approximate "Sinus of integer"
    0.0 (car (to-list (sin (arr <int> 0)))) 0.001)
  (test-approximate "Cosinus of single-precision floating point number"
    -1.0 (car (to-list (cos (arr <float> 3.141)))) 0.001)
  (test-approximate "Cosinus of double-precision floating point number"
    -1.0 (car (to-list (cos (arr <double> 3.141)))) 0.001)
  (test-approximate "Cosinus of integer"
    1.0 (car (to-list (cos (arr <int> 0)))) 0.001)
  (test-approximate "Tangent of single-precision floating point number"
    1.0 (car (to-list (tan (arr <float> 0.78525)))) 0.001)
  (test-approximate "Tangent of double-precision floating point number"
    1.0 (car (to-list (tan (arr <double> 0.78525)))) 0.001)
  (test-approximate "Tangent of integer"
    0.0 (car (to-list (tan (arr <int> 0)))) 0.001)
  (test-approximate "Arc sinus of single-precision floating point number"
    1.5705 (car (to-list (asin (arr <float> 1.0)))) 0.001)
  (test-approximate "Arc sinus of double-precision floating point number"
    1.5705 (car (to-list (asin (arr <double> 1.0)))) 0.001)
  (test-approximate "Arc cosinus of single-precision floating point number"
    3.141 (car (to-list (acos (arr <float> -1.0)))) 0.001)
  (test-approximate "Arc cosinus of double-precision floating point number"
    3.141 (car (to-list (acos (arr <double> -1.0)))) 0.001)
  (test-approximate "Arc tangent of single-precision floating point number"
    0.78525 (car (to-list (atan (arr <float> 1.0)))) 0.001)
  (test-approximate "Arc tangent of double-precision floating point number"
    0.78525 (car (to-list (atan (arr <double> 1.0)))) 0.001)
  (test-approximate "Arc tangent of two single-precision floating point numbers"
    2.356 (car (to-list (atan (arr <float> 1.0) (arr <float> -1.0)))) 0.001)
  (test-approximate "Arc tangent of two double-precision floating point numbers"
    2.356 (car (to-list (atan (arr <double> 1.0) (arr <double> -1.0)))) 0.001)
  (test-approximate "Arc tangent of single- and double-precision floating point number"
    2.356 (car (to-list (atan (arr <float> 1.0) (arr <double> -1.0)))) 0.001)
  (test-approximate "Exponentiation"
    2.718 (car (to-list (exp (arr <float> 1.0)))) 0.001))

(test-group "phi values"
  (test-eqv "Try phi function"
    42
    ((jit '()
      (lambda ()
        (let [(start (make-basic-block "start"))
              (block (make-basic-block "block"))]
        (llvm-begin
          (build-branch start)
          (position-builder-at-end start)
          (jit-let [(x (typed-constant <int> 42))]
            (build-branch block)
            (position-builder-at-end block)
            (jit-let [(phi (build-phi <int>))]
              (add-incoming phi start x)
              x)))))))))

(test-group "objects"
  (test-eqv "Size of object type"
    8 (size-of <obj>))
  (test-eq "Compile identity function for Scheme object"
    'abc ((jit (list <obj>) identity) 'abc))
  (test-equal "Array oF objects contains false by default"
    '(#f #f #f) (to-list (make (multiarray <obj> 1) #:shape '(3))))
  (test-equal "Type matching for Scheme objects"
    '(a b c) (to-list (arr <obj> a b c)))
  (test-equal "Unary plus for objects"
    '(2 3 5) (to-list (+ (arr <obj> 2 3 5))))
  (test-equal "Unary product for objects"
    '(2 3 5) (to-list (* (arr <obj> 2 3 5))))
  (test-equal "Unary minus for objects"
    '(-2 3 -5) (to-list (- (arr <obj> 2 -3 5))))
  (test-equal "Unary bitwise not for objects"
    '(-3 -4 -6) (to-list (~ (arr <obj> 2 3 5))))
  (test-equal "Add two object arrays"
    '(5 8 12) (to-list (+ (arr <obj> 2 3 5) (arr <obj> 3 5 7))))
  (test-equal "Add object array and integer"
    '(3 4 6) (to-list (+ (arr <obj> 2 3 5) 1)))
  (test-equal "Add integer and object array"
    '(3 4 6) (to-list (+ 1 (arr <obj> 2 3 5))))
  (test-equal "Subtract value from object array"
    '(1 2 4) (to-list (- (arr <obj> 2 3 5) 1)))
  (test-equal "Multiply object array with value"
    '(4 6 10) (to-list (* (arr <obj> 2 3 5) 2)))
  (test-equal "Divide object array by value"
    '(2 3 5) (to-list (/ (arr <obj> 4 6 10) 2)))
  (test-equal "Left-shift object array by value"
    '(4) (to-list (<< (arr <obj> 2) 1)))
  (test-skip 2)
  (test-equal "Right-shift object array by value"
    '(2) (to-list (>> (arr <obj> 4) 1)))
  (test-equal "Right-shift object array by different values"
    '(8 4 2) (to-list (>> 16 (arr <obj> 1 2 3))))
  (test-equal "Modulo object array by value"
    '(2 3 1) (to-list (% (arr <obj> 2 3 5) 4)))
  (test-equal "Bitwise and for objects"
    '(0 2 2) (to-list (& (arr <obj> 1 2 3) 2)))
  (test-equal "Bitwise or for objects"
    '(3 2 3) (to-list (| (arr <obj> 1 2 3) 2)))
  (test-equal "Minor value of objects"
    '(2 3 4) (to-list (minor (arr <obj> 2 3 5) 4)))
  (test-equal "Major value of objects"
    '(4 4 5) (to-list (major (arr <obj> 2 3 5) 4)))
  (for-each
    (lambda (type)
      (let [(value (ash (if  (signed? type) -1 1) (1- (bits type))))]
        (test-eqv (format #f "Convert ~a from ~a to object" value (class-name type))
          value ((jit (list type) (cut to-type <obj> <>)) value))))
    (list <ubyte> <byte> <usint> <sint> <uint> <int> <ulong> <long>))
  (test-eqv "Convert double-precision float to object"
    3.5 ((jit (list <double>) (cut to-type <obj> <>)) 3.5))
  (test-eqv "Convert single-precision float to object"
    3.5 ((jit (list <float>) (cut to-type <obj> <>)) 3.5))
  (test-equal "Convert boolean to object"
    '(#f #t) (map (jit (list <bool>) (cut to-type <obj> <>)) '(#f #t)))
  (for-each
    (lambda (type)
      (let [(value (ash (if  (signed? type) -1 1) (1- (bits type))))]
        (test-eqv (format #f "Convert ~a from object to ~a" value (class-name type))
          value ((jit (list <obj>) (cut to-type type <>)) value))))
    (list <ubyte> <byte> <usint> <sint> <uint> <int> <ulong> <long>))
  (test-eqv "Convert object to double-precision float"
    3.5 ((jit (list <obj>) (cut to-type <double> <>)) 3.5))
  (test-eqv "Convert single-precision float to object"
    3.5 ((jit (list <obj>) (cut to-type <float> <>)) 3.5))
  (test-equal "Convert object to boolean"
    '(#f #t) (map (jit (list <obj>) (cut to-type <bool> <>)) '(#f #t))))

(test-group "index arrays"
  (test-equal "shape of index array"
    '(2 3 5) (shape (indices 2 3 5)))
  (test-equal "indices in 1D array"
    '(0 1 2) (to-list (indices 3)))
  (test-equal "2D index array"
    '((0 1 2) (3 4 5)) (to-list (indices 2 3))))

(test-group "comparison"
  (test-assert "equal arrays"
    (equal? (arr 2 3 5) (arr 2 3 5)))
  (test-assert "differing shape"
    (not (equal? (arr 2 3) (arr 2 3 5))))
  (test-assert "different elements"
    (not (equal? (arr 2 3 5) (arr 2 4 5)))))

(test-group "warps"
  (test-equal "trivial 1D warp"
    '(2 3 5) (to-list (warp (arr 2 3 5) (arr 0 1 2))))
  (test-equal "mirroring 1D warp"
    '(5 3 2) (to-list (warp (arr 2 3 5) (arr 2 1 0))))
  (test-equal "use strides of source array"
    '(2 3 5) (to-list (warp (arr <int> 2 3 5) (arr 0 1 2))))
  (test-equal "use strides of index array"
    '(2 3 5) (to-list (warp (arr 2 3 5) (arr <int> 0 1 2))))
  (test-equal "use shape of index array"
    '(2 3) (to-list (warp (arr 2 3 5) (arr 0 1))))
  (test-equal "warp with two index arrays"
    '(2 3 13) (to-list (warp (arr (2 3 5) (7 11 13)) (arr 0 1 2) (arr 0 0 1))))
  (test-equal "warp with 2D result"
    '((2 3 5) (13 11 7)) (to-list (warp (arr 2 3 5 7 11 13) (arr (0 1 2) (5 4 3)))))
  (test-equal "warp with 2D and 1D index array"
    '((13 13 13)) (to-list (warp (arr (2 3 5) (7 11 13)) (arr 2) (arr (1 1 1)))))
  (test-equal "warp with 1D and 2D index array"
    '((13 13 13)) (to-list (warp (arr (2 3 5) (7 11 13)) (arr (2 2 2)) (arr 1))))
  (test-equal "warp with arrays as elements"
    '((11 13) (2 3) (5 7)) (to-list (warp (arr (2 3) (5 7) (11 13)) (arr 2 0 1)))))

(test-group "reshape"
  (test-equal "trivial reshape operation"
    '(2 3 5) (to-list (reshape (arr 2 3 5) '(3))))
  (test-equal "reshape 2D array"
    '((2 3) (5 7) (11 13)) (to-list (reshape (arr (2 3 5) (7 11 13)) '(3 2))))
  (test-equal "ensure compact array before reshaping"
    '((2 3 5) (7 11 13)) (to-list (reshape (roll (arr (2 5 11) (3 7 13))) '(2 3)))))

(test-group "histograms"
  (test-equal "shape of histogram"
    '(5) (shape (histogram (list 5) (arr 2 3 4))))
  (test-equal "initialize histogram"
    '(0 0 0 0 0) (to-list (histogram '(5) (make (multiarray <ubyte> 1) #:shape '(0)))))
  (test-equal "1D histogram of array"
    '(0 0 1 2 3) (to-list (histogram '(5) (arr 2 3 3 4 4 4) )))
  (test-equal "1D histogram of array of integers"
    '(0 0 1 2 3) (to-list (histogram '(5) (arr <int> 2 3 3 4 4 4) )))
  (test-equal "2D histogram y axis"
    '((0 0 0) (1 0 0)) (to-list (histogram '(2 3) (arr 1) (arr 0))))
  (test-equal "2D histogram x axis"
    '((0 1 0) (0 0 0)) (to-list (histogram '(2 3) (arr 0) (arr 1))))
  (test-equal "1D histogram of 2D array"
    '(0 0 1 2 3) (to-list (histogram '(5) (arr (2 3 3) (4 4 4)))))
  (test-equal "2D histogram of 2D and 1D array"
    '((0 0 0) (1 0 0)) (to-list (histogram '(2 3) (arr (1)) (arr 0))))
  (test-equal "2D histogram of 1D and 2D array"
    '((0 0 0) (1 0 0)) (to-list (histogram '(2 3) (arr 1) (arr (0))))))

(test-group "masking"
  (test-equal "shape when mask is all true"
    '(3) (shape (mask (arr 2 3 5) (arr #t #t #t))))
  (test-equal "shape with some values masked out"
    '(2) (shape (mask (arr 2 3 5) (arr #t #f #t))))
  (test-equal "all values selected"
    '(2 3 5) (to-list (mask (arr <int> 2 3 5) (arr #t #t #t))))
  (test-equal "some values selected"
    '(2 5) (to-list (mask (arr <int> 2 3 5) (arr #t #f #t))))
  (test-equal "mask 2D array"
    '(2 11 13) (to-list (mask (arr (2 3 5) (7 11 13)) (arr (#t #f #f) (#f #t #t)))))
  (test-equal "allocate result for mask with arrays as elements"
    '(3 2) (shape (mask (arr (2 3) (5 7) (11 13) (17 19)) (arr #t #f #t #t))))
  (test-equal "mask with arrays as elements"
    '((2 3) (11 13) (17 19)) (to-list (mask (arr (2 3) (5 7) (11 13) (17 19)) (arr #t #f #t #t)))))

(test-group "unmasking"
  (test-equal "result when mask is true"
    '(2 3 5) (to-list (unmask (arr 2 3 5) (arr #t #t #t))))
  (test-equal "restore original shape"
    '((2 3 5) (7 11 13)) (to-list (unmask (arr 2 3 5 7 11 13) (arr (#t #t #t) (#t #t #t)))))
  (test-equal "restore partially masked array"
    '(0 0 2 0 3 5) (to-list (unmask (arr 2 3 5) (arr #f #f #t #f #t #t))))
  (test-equal "restore masked array with arrays as elements"
    '((0 0) (2 3) (3 5)) (to-list (unmask (arr (2 3) (3 5)) (arr #f #t #t)))))

(test-group "object methods"
  (test-equal "elementwise negation for objects"
    '(#t #f) (to-list (! (arr <obj> #f #t))))
  (test-equal "object greater than"
    '(#f #f #t) (to-list (gt (arr <obj> 1 2 3) 2)))
  (test-equal "object greater equal"
    '(#f #t #t) (to-list (ge (arr <obj> 1 2 3) 2)))
  (test-equal "object lower than"
    '(#t #f #f) (to-list (lt (arr <obj> 1 2 3) 2)))
  (test-equal "object lower equal"
    '(#t #t #f) (to-list (le (arr <obj> 1 2 3) 2)))
  (test-equal "object equal"
    '(#f #t #f) (to-list (eq (arr <obj> 1 2 3) 2))))

(test-end "aiscm core")
