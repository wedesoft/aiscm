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
             (rnrs bytevectors)
             (oop goops)
             (system foreign)
             (aiscm basictype)
             (aiscm llvm))

(test-begin "aiscm llvm")

(test-begin "module")
  (test-equal "Create LLVM instance"
    <llvm> (class-of (make-llvm-module)))
  (test-assert "Destroy LLVM instance"
    (unspecified? (destroy (make-llvm-module))))
  (test-assert "LLVM module slot defined"
    (slot-ref (make-llvm-module) 'llvm-module))
(test-end "module")

(test-begin "constant values")
  (test-equal "Build an integer value"
    <llvm-value> (class-of ((make-constant int32 42) #f)))
  (for-each
    (lambda (type bits)
      (test-equal (format #f "Get type of ~a-bit integer value" bits)
        type (get-type ((make-constant type 42) #f))))
    (list int8 int16 int32 int64)
    '(8 16 32 64))
  (for-each
    (lambda (unsigned-type signed-type bits)
      (test-equal (format #f "Type of ~a-bit value ignores signed-ness" bits)
        signed-type (get-type ((make-constant unsigned-type 42) #f))))
    (list uint8 uint16 uint32 uint64)
    (list int8 int16 int32 int64)
    '(8 16 32 64))
  (test-equal "Get type of double-precision floating point value"
    double (get-type ((make-constant double (exp 1)) #f)))
  (test-equal "Get type of single-precision floating point value"
    float (get-type ((make-constant float (exp 1)) #f)))
(test-end "constant values")

(test-begin "functions")
  (test-equal "Create LLVM function"
    <llvm-function> (class-of (let [(mod (make-llvm-module))] (make-function mod void "function"))))
  (let [(mod (make-llvm-module))]
    (test-equal "Keep LLVM instance alive"
      mod (slot-ref (make-function mod void "function") 'module)))
  (test-assert "Compile, verify, and run empty function"
    (unspecified?
      (let* [(mod  (make-llvm-module))
             (fun  (make-function mod void "empty"))]
        ((function-ret) fun)
        (llvm-compile mod)
        ((llvm-func mod fun)))))
  (test-assert "Dump module containing a function"
    (unspecified?
      (let* [(mod  (make-llvm-module))
             (fun  (make-function mod void "empty"))]
        ((function-ret) fun)
        (llvm-dump mod))))
  (test-error "Throw error if module is not valid"
    'misc-error
    (let* [(mod  (make-llvm-module))
           (fun  (make-function mod void "incomplete"))]
      (llvm-compile llvm)))
  (test-error "Throw error when attempting to compile twice"
    'misc-error
    (let* [(mod  (make-llvm-module))
           (fun  (make-function mod void "empty"))]
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
    (list int8 int16 int32 int64 uint8 uint16 uint32 uint64)
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
    (list float double)
    (list "single" "double"))
(test-end "functions")

(test-begin "pointers")
  (for-each (lambda (value type name)
    (test-equal (format #f "Read ~a value from memory" name)
      value
      (let* [(data #vu8(2 3 5 7))
             (mod  (make-llvm-module))
             (fun  (make-function mod type "read_mem"))]
        ((function-ret (function-load type (make-constant-pointer (bytevector->pointer data)))) fun)
        (llvm-compile mod)
        ((llvm-func mod fun)))))
    '(2 770)
    (list int8 int16)
    '("byte" "short integer"))
  (for-each (lambda (data value type name)
    (test-equal (format #f "Write ~a to memory" name)
      #vu8(2 3 5 7)
      (let* [(mod  (make-llvm-module))
             (fun  (make-function mod void "write_mem"))]
        ((function-store type (make-constant type value) (make-constant-pointer (bytevector->pointer data))) fun)
        ((function-ret) fun)
        (llvm-compile mod)
        ((llvm-func mod fun))
        data)))
    (list #vu8(0 3 5 7) #vu8(0 0 5 7))
    '(2 770)
    (list int8 int16)
    '("byte" "short integer"))
(test-end "pointers")

(test-begin "method arguments")
  (test-assert "Declare a function which accepts arguments"
    (let [(mod (make-llvm-module))]
      (make-function mod int "with_arg" int)))
  (test-assert "Call a function accepting an argument"
    (let* [(mod  (make-llvm-module))
           (fun  (make-function mod void "accept_arg" int))]
      ((function-ret) fun)
      (llvm-compile mod)
      ((llvm-func mod fun) 42)))
  (for-each (lambda (value type name)
    (test-equal (format #f "Compile, verify, and run ~a identity function" name)
      value
      (let* [(mod  (make-llvm-module))
             (fun  (make-function mod type "int_identity" type))]
        ((function-ret (function-param 0)) fun)
        (llvm-compile mod)
        ((llvm-func mod fun) value))))
    '(42 0.5)
    (list int double)
    '("integer" "floating-point"))
(test-end "method arguments")

(test-begin "unary operation")
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
    (list uint8 int double))
(test-end "unary operation")

(test-begin "binary expressions")
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
    (list int int int double double double))
(test-end "binary expressions")

(test-begin "sequential program")
  (test-eqv "Sequence with one statement"
    3 ((llvm-sequential 1+) 2))
  (test-eqv "Sequence with two statements returns result of last statement"
    2 ((llvm-sequential 1+ 1-) 3))
  (test-eqv "All statements are executed"
    2 (let [(x 0)] ((llvm-sequential (lambda (v) (set! x v)) (lambda (v) (+ v x))) 1)))
(test-end "sequential program")

(test-begin "convenience wrapper")
  (test-assert "Define empty function using convenience wrapper"
    (unspecified? ((llvm-wrap void '() function-ret))))
  (test-eqv "Define constant function using convenience wrapper"
    42
    ((llvm-wrap int '() (lambda () (function-ret (make-constant int 42))))))
  (test-eqv "Define identity function using convenience wrapper"
    42
    ((llvm-wrap int (list int) (lambda (value) (function-ret value))) 42))
  (test-eqv "Define negating function using convenience wrapper"
    -42
    ((llvm-wrap int (list int) (lambda (value) (function-ret (llvm-neg value)))) 42))
  (test-eqv "Define addition function using convenience wrapper"
    36
    ((llvm-wrap int (list int int) (lambda (value-a value-b) (function-ret (llvm-add value-a value-b)))) 21 15))
  (test-equal "Define function with side-effect but no return value"
    #vu8(42)
    (let* [(data    #vu8(0))
           (pointer (make-constant-pointer (bytevector->pointer data)))]
      ((llvm-wrap void (list int8) (lambda (value) (llvm-sequential (function-store int8 value pointer) (function-ret)))) 42)
      data))
  (test-eqv "Pass pointer argument"
    42
    (let* [(data    #vu8(42))
           (pointer (pointer-address (bytevector->pointer data)))]
      ((llvm-wrap int8 (list int64) (lambda (value) (function-ret (function-load int8 value)))) pointer)))
(test-end "convenience wrapper")

(test-begin "typecast")
  (test-equal "Zero-extend integer"
    255
    (let* [(data #vu8(255))
           (pointer (pointer-address (bytevector->pointer data)))]
      ((llvm-wrap uint32 (list int64) (lambda (value) (function-ret (llvm-zext uint32 (function-load uint8 value))))) pointer)))
  (test-equal "Sign-extend integer"
    -1
    (let* [(data #vu8(255))
           (pointer (pointer-address (bytevector->pointer data)))]
      ((llvm-wrap int32 (list int64) (lambda (value) (function-ret (llvm-sext int32 (function-load int8 value))))) pointer)))
  (test-equal "Truncate integer"
    #xcd ((llvm-wrap uint8 (list uint16) (lambda (value) (function-ret (llvm-trunc int8 value)))) #xabcd))
(test-end "typecast")

(test-begin "local variables")
  (test-eqv "Empty list of local variables"
    3 ((llvm-let* [] 1+) 2))
  (test-eqv "Use function defining a local variable"
    3 ((llvm-let* [(result 1+)] result) 2))
  (test-eqv "Environment with two statements returns result of last statement"
    2 ((llvm-let* [] 1+ 1-) 3))
  (test-eqv "All statements are executed"
    2 (let [(x 0)] ((llvm-let* [] (lambda (v) (set! x v)) (lambda (v) (+ v x))) 1)))
  (test-eqv "Intermediate results are cached"
    1 (let [(x 0)] ((llvm-let* [(result (lambda (v) (set! x (+ v x)) x))] result result) 1)))
(test-end "local variables")

(test-begin "unary expression")
  (test-equal "unary minus invokes llvm negation"
    -42
    (let* [(mod (make-llvm-module))
           (fun (make-function mod int "op" int))]
      ((function-ret (get (- (make <int> #:value (function-param 0))))) fun)
      (llvm-compile mod)
      ((llvm-func mod fun) 42)))
  (for-each (lambda (type)
    (test-equal (format #f "unary minus should preserve ~a type" type)
      type (class-of (- (make type #:value (function-param 0))))))
    (list <ubyte> <byte> <usint> <sint> <uint> <int> <ulong> <long>))
(test-end "unary expression")

(test-begin "type inference")
  (test-equal "identity function with integer"
    (make <int> #:value 42) ((llvm-typed (list <int>) identity) (make <int> #:value 42)))
  (test-equal "identity function with short integer"
    (make <sint> #:value 42) ((llvm-typed (list <sint>) identity) (make <sint> #:value 42)))
(test-end "type inference")
(test-end "aiscm llvm")
