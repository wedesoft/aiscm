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
             (rnrs bytevectors)
             (oop goops)
             (system foreign)
             (aiscm basictype)
             (aiscm llvm))

(test-begin "aiscm llvm")

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
    '(2 100 5 2.5 5.75 2.5)
    '(3 30 7 3.25 3.25 3.25)
    (list llvm-add llvm-sub llvm-mul llvm-fadd llvm-fsub llvm-fmul)
    '(5 70 35 5.75 2.5 8.125)
    (list llvm-int32 llvm-int32 llvm-int32 llvm-double llvm-double llvm-double)))

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
    42 ((llvm-typed (list <int>) (cut to-type <int> <>)) 42))
  (test-equal "truncating integer conversion"
    #xcd ((llvm-typed (list <uint>) (cut to-type <ubyte> <>)) #xabcd))
  (test-equal "zero-extending integer conversion"
    200 ((llvm-typed (list <ubyte>) (lambda (value) (to-type <int> (to-type <ubyte> value)))) 200))
  (test-equal "zero-extending integer conversion"
    200 ((llvm-typed (list <ubyte>) (lambda (value) (to-type <int> (to-type <ubyte> value)))) 200))
  (test-equal "sign-extending integer conversion"
    -42 ((llvm-typed (list <byte>) (lambda (value) (to-type <int> (to-type <byte> value)))) -42)))

(test-group "type inference"
  (test-equal "identity function with integer"
    42 ((llvm-typed (list <int>) identity) 42))
  (test-equal "identity function returning true"
    #t ((llvm-typed (list <bool>) identity) #t))
  (test-equal "identity function returning false"
    #f ((llvm-typed (list <bool>) identity) #f))
  (test-equal "compact integer negation"
    -42 ((llvm-typed (list <int>) -) 42))
  (test-equal "sum of two integers"
    5 ((llvm-typed (list <int> <int>) +) 2 3))
  (test-equal "sum of unsigned byte and byte"
    382 ((llvm-typed (list <ubyte> <byte>) +) 255 127)))

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
    42.5 ((llvm-typed (list <float>) (cut to-type <double> <>)) 42.5)))

(test-group "convert between integer and floating-point"
  (test-equal "signed byte to float"
    -42.0 ((llvm-typed (list <byte>) (cut to-type <float> <>)) -42))
  (test-equal "unsigned byte to float"
    200.0 ((llvm-typed (list <ubyte>) (cut to-type <float> <>)) 200))
  (test-equal "convert float to signed integer"
    -42 ((llvm-typed (list <int>) (lambda (value) (to-type <int> (to-type <double> value)))) -42))
  (test-equal "convert float to unsigned int"
    200 ((llvm-typed (list <uint>) (lambda (value) (to-type <uint> (to-type <double> value)))) 200)))

(test-group "integer unary expressions"
  (for-each (lambda (op result)
    (test-equal (format #f "(~a 42) should be ~a" (procedure-name op) result)
      result ((llvm-typed (list <int>) op) 42)))
    (list ~ -)
    '(-43 -42)))

(test-group "floating-point unary expression"
  (test-equal "(- 42.5) single-precision should be -42.5"
    -42.5 ((llvm-typed (list <float>) -) 42.5))
  (test-equal "(- 42.5) double-precision should be -42.5"
    -42.5 ((llvm-typed (list <double>) -) 42.5)))

(test-group "integer binary expressions"
  (for-each (lambda (op result)
    (test-equal (format #f "(~a 2 3) should be ~a" (procedure-name op) result)
      result ((llvm-typed (list <int> <int>) op) 2 3)))
    (list + - *)
    '(5 -1 6)))

(test-group "floating-point binary expression"
  (for-each (lambda (value-a value-b op result)
    (test-equal (format #f "(~a ~a ~a) should be ~a" (procedure-name op) value-a value-b result)
      result ((llvm-typed (list (if (integer? value-a) <int> <float>)
                                (if (integer? value-b) <int> <float>))
                          op) value-a value-b)))
    '(2.5 2.5 2 3.75 2 2.5 1.5 2 1.25)
    '(3.75 3 3.75 2.5 1.5 1 2.5 1.25 2)
    (list + + + - - - * * *)
    '(6.25 5.5 5.75 1.25 0.5 1.5 3.75 2.5 2.5)))

(test-group "constant conversions"
  (test-eqv "add integer constant to value"
    5 ((llvm-typed (list <int>) (lambda (x) (+ x 3))) 2))
  (test-eqv "add value to integer constant"
    5 ((llvm-typed (list <int>) (lambda (x) (+ 2 x))) 3))
  (test-eqv "subtract integer constant from value"
    2 ((llvm-typed (list <int>) (lambda (x) (- x 3))) 5))
  (test-eqv "subtract value from integer constant"
    3 ((llvm-typed (list <int>) (lambda (x) (- 5 x))) 2))
  (test-eqv "add floating-point constant to value"
    5.5 ((llvm-typed (list <float>) (lambda (x) (+ x 3))) 2.5))
  (test-eqv "add floating-point constant to integer value"
    5.5 ((llvm-typed (list <int>) (lambda (x) (+ x 3.5))) 2))
  (test-eqv "add complex number to integer value"
    7+3i ((llvm-typed (list <int>) (lambda (x) (+ x 2+3i))) 5))
  (test-eqv "add complex number to complex value"
    7+10i ((llvm-typed (list <complex<float>>) (lambda (x) (+ x 2+3i))) 5+7i)))

(test-group "composite types"
  (test-eqv "return real part of complex number"
    2.5 ((llvm-typed (list <complex<float>>) real-part) 2.5+3.25i))
  (test-eqv "return imaginary part of complex number"
    3.25 ((llvm-typed (list <complex<float>>) imag-part) 2.5+3.25i))
  (test-eqv "complex single-precision identity"
    2+3i ((llvm-typed (list <complex<float>>) identity) 2+3i))
  (test-eqv "complex double-precision identity"
    2+3i ((llvm-typed (list <complex<double>>) identity) 2+3i))
  (test-eqv "compose single-precision complex number"
    2+3i ((llvm-typed (list <float> <float>) complex) 2 3))
  (test-eqv "compose double-precision complex number"
    2+3i ((llvm-typed (list <double> <double>) complex) 2 3))
  (test-eqv "compose complex number from different precision numbers"
    2+3i ((llvm-typed (list <float> <double>) complex) 2 3))
  (test-eqv "complex negation"
    -2-3i ((llvm-typed (list <complex<double>>) -) 2+3i))
  (test-eqv "complex plus"
    7+10i ((llvm-typed (list <complex<double>> <complex<double>>) +) 2+3i 5+7i))
  (test-eqv "add scalar to complex value"
    7+3i ((llvm-typed (list <complex<double>> <int>) +) 2+3i 5))
  (test-eqv "add complex value to scalar"
    5+5i ((llvm-typed (list <float> <complex<float>>) +) 2 3+5i))
  (test-eqv "complex minus"
    -3-4i ((llvm-typed (list <complex<double>> <complex<double>>) -) 2+3i 5+7i))
  (test-eqv "subtract scalar from complex value"
    -3+3i ((llvm-typed (list <complex<double>> <int>) -) 2+3i 5))
  (test-eqv "subtract complex value from scalar"
    -1-5i ((llvm-typed (list <float> <complex<float>>) -) 2 3+5i))
  (test-eqv "real part of real number"
    5.5 ((llvm-typed (list <float>) real-part) 5.5))
  (test-eqv "imaginary part of real number"
    0.0 ((llvm-typed (list <float>) imag-part) 5.5))
  (test-eqv "convert float to complex"
    5.0+0.0i ((llvm-typed (list <float>) (cut to-type <complex<float>> <>)) 5))
  (test-eqv "change precision of floating point number"
    2.0+3.0i ((llvm-typed (list (complex <float>)) (cut to-type <complex<double>> <>)) 2+3i)))

(test-group "method calls"
  (test-equal "call libc's fabsf method"
    1.25
    ((llvm-wrap (list llvm-float)
                (lambda args (cons llvm-float (function-ret (llvm-call llvm-float "fabsf" (list llvm-float) args)))))
     -1.25))
  (test-equal "call libc's atan2 method"
    0.0
    ((llvm-wrap (list llvm-double llvm-double)
                (lambda args (cons llvm-double (function-ret (llvm-call llvm-double "atan2" (list llvm-double llvm-double) args)))))
     0.0 1.0)))

(test-group "pointers"
  (test-equal "Pointer identity function"
    (make-pointer 123) ((llvm-typed (list (pointer <int>)) identity) (make-pointer 123)))
  (test-eqv "Fetch pointer value"
    42 ((llvm-typed (list (pointer <byte>)) fetch) (bytevector->pointer #vu8(42 63))))
  (test-equal "Typecast when writing to pointer target"
    #vu8(42 3 5 7)
    (let [(data #vu8(2 3 5 7))]
      ((llvm-typed (list (pointer <byte>) <int>) (lambda (ptr value) (store ptr value)))
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
    #t ((llvm-typed '() (const (typed-constant <bool> #t)))))
  (test-equal "integer constant"
    42 ((llvm-typed '() (const (typed-constant <int> 42)))))
  (test-equal "floating-point constant"
    2.5 ((llvm-typed '() (const (typed-constant <double> 2.5)))))
  (test-equal "complex constant"
    2+3i ((llvm-typed '() (const (typed-constant <complex<float>> 2+3i))))))

(test-group "typed store/fetch"
  (let* [(data #vu8(0 3 5 7))
         (ptr  (typed-pointer <byte> (bytevector->pointer data)))]
    (test-equal "write byte to memory"
      #vu8(2 3 5 7)
      (begin ((llvm-typed (list <byte>) (lambda (value) (store ptr value))) 2) data)))
  (let* [(data #vu8(0 3 5 7))
         (ptr  (typed-pointer <byte> (bytevector->pointer data)))]
    (test-assert "storing a value returns no value"
      (unspecified? ((llvm-typed (list <byte>) (lambda (value) (store ptr value))) 2))))
  (let* [(data #vu8(2 3 5 7))
         (ptr  (typed-pointer <byte> (bytevector->pointer data)))]
    (test-eqv "read byte from memory"
      2 ((llvm-typed '() (lambda () (fetch ptr))))))
  (let* [(data #vu8(1 2 3 4 5 6 7 8 9 10))
         (ptr  (typed-pointer (complex <float>) (bytevector->pointer data)))]
    (test-equal "write complex number to memory"
      #vu8(0 0 0 64 0 0 64 64 9 10)
      (begin ((llvm-typed (list <complex<float>>) (lambda (value) (store ptr value))) 2+3i) data)))
  (let* [(data #vu8(0 0 0 64 0 0 64 64))
         (ptr  (typed-pointer (complex <float>) (bytevector->pointer data)))]
    (test-eqv "read complex number from memory"
      2+3i ((llvm-typed '() (lambda () (fetch ptr)))))))

(test-group "instruction sequence"
  (test-eqv "test single instruction"
    42 ((llvm-typed (list <int>) (lambda (value) (llvm-begin value))) 42))
  (test-eqv "test two instructions"
    42 ((llvm-typed (list <int>) (lambda (value) (llvm-begin value value))) 42))
  (let* [(data #vu8(0 0 0 0))
         (ptr  (typed-pointer <int> (bytevector->pointer data)))]
    (test-eqv "ensure both instructions are executed"
      42 ((llvm-typed (list <int>) (lambda (value) (llvm-begin (store ptr value) (fetch ptr)))) 42))))

(define-class <testcontainer> ()
              (testcontent #:init-keyword #:testcontent #:getter testcontent))
(define (make-testcontainer testcontent) (make <testcontainer> #:testcontent testcontent))
(define-structure testcontainer make-testcontainer (testcontent))
(define-uniform-constructor testcontainer)
(test-group "operations for custom composite type"
  (test-assert "compile identity operation for composite type"
    (llvm-typed (list (testcontainer <int>)) identity))
  (test-eqv "run identity operation for composite type"
    42 (testcontent ((llvm-typed (list (testcontainer <int>)) identity) (make-testcontainer 42))))
  (test-assert "compile constructor of test container"
    (llvm-typed (list <int>) testcontainer))
  (test-equal "Uniform container performs type coercion"
    (complex <float>) (class-of (complex (typed-constant <int> 2) (typed-constant <float> 3)))))

(define-class <testmixed> ()
              (test-a #:init-keyword #:test-a #:getter test-a)
              (test-b #:init-keyword #:test-b #:getter test-b))
(define (make-testmixed test-a test-b) (make <testmixed> #:test-a test-a #:test-b test-b))
(define-structure testmixed make-testmixed (test-a test-b))
(define-mixed-constructor testmixed)
(test-group "operations for mixed composite type"
  (let* [(data #vu8(3 5 7))
         (ptr  (typed-pointer (testmixed <sint> <byte>) (bytevector->pointer data)))]
    (test-eqv "read first value of mixed variable"
      1283 (test-a ((llvm-typed '() (lambda () (fetch ptr))))))
    (test-eqv "read second value of mixed variable"
      7 (test-b ((llvm-typed '() (lambda () (fetch ptr)))))))
    (let* [(data #vu8(1 2 3 4))
           (ptr  (typed-pointer (testmixed <sint> <byte>) (bytevector->pointer data)))]
      (test-equal "write composite value to memory"
        #vu8(3 5 7 4)
        (begin
          ((llvm-typed (list (testmixed <sint> <byte>)) (lambda (value) (store ptr value)))
           (make-testmixed 1283 7))
          data)))
  (test-equal "Mixed constructor preserves types of components"
    (testmixed <sint> <byte>) (class-of (testmixed (typed-constant <sint> 2) (typed-constant <byte> 3)))))

(test-group "comparison"
  (test-eqv "unsigned less-than"
    #t ((llvm-typed (list <ubyte> <ubyte>) lt) 120 140))
  (test-eqv "not unsigned less-than"
    #f ((llvm-typed (list <ubyte> <ubyte>) lt) 120 120))
  (test-eqv "signed less-than"
    #t ((llvm-typed (list <byte> <ubyte>) lt) -100 100))
  (test-eqv "not signed less-than"
    #f ((llvm-typed (list <ubyte> <byte>) lt) 100 -100))
  (test-eqv "type coercion for signed less-than"
    #t ((llvm-typed (list <byte> <ubyte>) lt) -1 255))
  (test-eqv "unsigned less-than or equal"
    #t ((llvm-typed (list <ubyte> <ubyte>) le) 120 120))
  (test-eqv "not unsigned less-than or equal"
    #f ((llvm-typed (list <ubyte> <ubyte>) le) 120 119))
  (test-eqv "signed less-than or equal"
    #t ((llvm-typed (list <byte> <ubyte>) le) -100 100))
  (test-eqv "not signed less-than or equal"
    #f ((llvm-typed (list <ubyte> <byte>) le) 100 -100))
  (test-eqv "unsigned greater-than"
    #t ((llvm-typed (list <ubyte> <ubyte>) gt) 140 120))
  (test-eqv "not unsigned greater-than"
    #f ((llvm-typed (list <ubyte> <ubyte>) gt) 120 120))
  (test-eqv "signed greater-than"
    #t ((llvm-typed (list <ubyte> <byte>) gt) 100 -100))
  (test-eqv "not signed greater-than"
    #f ((llvm-typed (list <byte> <ubyte>) gt) -100 100))
  (test-eqv "unsigned greater-than or equal"
    #t ((llvm-typed (list <ubyte> <ubyte>) ge) 120 120))
  (test-eqv "not unsigned greater-than or equal"
    #f ((llvm-typed (list <ubyte> <ubyte>) ge) 119 120))
  (test-eqv "signed greater-than or equal"
    #t ((llvm-typed (list <ubyte> <byte>) ge) 100 -100))
  (test-eqv "not signed greater-than or equal"
    #f ((llvm-typed (list <byte> <ubyte>) ge) -100 100))
  (test-eqv "floating-point less-than"
    #t ((llvm-typed (list <float> <float>) lt) 2.5 3.0))
  (test-eqv "floating-point not less-than"
    #f ((llvm-typed (list <float> <float>) lt) 3.0 3.0))
  (test-eqv "compare floating-point and integer"
    #t ((llvm-typed (list <float> <int>) lt) 2.5 3))
  (test-eqv "compare integer and floating-point"
    #t ((llvm-typed (list <int> <float>) lt) 2 3))
  (test-eqv "floating-point less-than or equal"
    #t ((llvm-typed (list <float> <float>) le) 3.0 3.0))
  (test-eqv "floating-point not less-than or equal"
    #f ((llvm-typed (list <float> <float>) le) 3.5 3.0))
  (test-eqv "floating-point greater-than"
    #t ((llvm-typed (list <float> <float>) gt) 2.5 2.0))
  (test-eqv "not floating-point greater-than"
    #f ((llvm-typed (list <float> <float>) gt) 3.5 3.5))
  (test-eqv "floating-point greater-than or equal"
    #t ((llvm-typed (list <float> <float>) ge) 2.5 2.5))
  (test-eqv "not floating-point greater-than or equal"
    #f ((llvm-typed (list <float> <float>) ge) 1.0 1.5)))

(test-group "local variables"
  (test-eq "Empty variable list declaration"
    <int> (class-of (with-llvm-values () (typed-constant <int> 42))))
  (test-eqv "Return result of last instruction"
    <sint> (class-of (with-llvm-values () (typed-constant <byte> 20) (typed-constant <sint> 42))))
(let* [(data #vu8(0 0 0 0))
       (ptr  (typed-pointer <int> (bytevector->pointer data)))]
    (test-eqv "ensure both instructions are executed"
      42 ((llvm-typed (list <int>) (lambda (value) (with-llvm-values [] (store ptr value) (fetch ptr)))) 42)))
  (test-eqv "define variable"
    42 ((llvm-typed (list <int>) (lambda (value) (with-llvm-values [x] (llvm-set x value) x))) 42)))

(test-group "basic blocks and branch instructions"
  (test-equal "branch instruction"
    42
    ((llvm-typed (list <int>)
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
    (map (llvm-typed (list <int>) (lambda (x) (llvm-if (lt x 0) (- x) x))) '(3 -5)))
  (test-eqv "scalar coercion"
    3.5 ((llvm-typed (list <int> <float>) (lambda (x y) (llvm-if (lt x y) x y))) 5 3.5))
  (test-eqv "conditional with composite value"
    2.0+3.0i
    ((llvm-typed (list (complex <float>) (complex <float>)) (lambda (x y) (llvm-if (lt (real-part x) (real-part y)) x y)))
     2+3i 5+7i)))

(test-group "alloca for loop variables"
  (test-eqv "Allocate a variable on the stack"
    42
    ((llvm-typed (list <int>)
                 (lambda (x)
                   (with-llvm-values (ptr)
                     (llvm-set ptr (typed-alloca <int>))
                     (store ptr x)
                     (fetch ptr)))) 42))
  (test-eqv "While loop"
    10
    ((llvm-typed (list <int>)
      (lambda (n)
        (with-llvm-values (i)
          (llvm-set i (typed-alloca <int>))
          (store i (typed-constant <int> 0))
          (llvm-while (lt (fetch i) n)
            (store i (+ 1 (fetch i))))
          (fetch i)))) 10)))

(test-end "aiscm llvm")
