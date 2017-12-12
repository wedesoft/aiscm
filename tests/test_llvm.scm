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
             (aiscm llvm))

(test-begin "aiscm llvm")

(test-begin "module")
  (test-equal "Create LLVM instance"
    <llvm> (class-of (make-module)))
  (test-assert "Destroy LLVM instance"
    (unspecified? (destroy (make-module))))
  (test-assert "LLVM module slot defined"
    (slot-ref (make-module) 'llvm-module))
(test-end "module")

(test-begin "constant values")
  (test-equal "Build an integer value"
    <llvm-value> (class-of (make-constant int32 42)))
  (for-each
    (lambda (type bits)
      (test-equal (format #f "Get type of ~a-bit integer value" bits)
        type (get-type (make-constant type 42))))
    (list int8 int16 int32 int64)
    '(8 16 32 64))
  (for-each
    (lambda (unsigned-type signed-type bits)
      (test-equal (format #f "Type of ~a-bit value ignores signed-ness" bits)
        signed-type (get-type (make-constant unsigned-type 42))))
    (list uint8 uint16 uint32 uint64)
    (list int8 int16 int32 int64)
    '(8 16 32 64))
  (test-equal "Get type of double-precision floating point value"
    double (get-type (make-constant double (exp 1))))
  (test-equal "Get type of single-precision floating point value"
    float (get-type (make-constant float (exp 1))))
(test-end "constant values")

(test-begin "functions")
  (test-equal "Create LLVM function"
    <llvm-function> (class-of (let [(mod (make-module))] (make-function mod void "function"))))
  (let [(mod (make-module))]
    (test-equal "Keep LLVM instance alive"
      mod (slot-ref (make-function mod void "function") 'module)))
  (test-assert "Compile, verify, and run empty function"
    (unspecified?
      (let* [(mod  (make-module))
             (fun  (make-function mod void "empty"))]
        (function-ret fun)
        (llvm-compile mod)
        ((llvm-func mod fun)))))
  (test-assert "Dump module containing a function"
    (unspecified?
      (let* [(mod  (make-module))
             (fun  (make-function mod void "empty"))]
        (function-ret fun)
        (llvm-dump mod))))
  (test-error "Throw error if verification of module failed"
    'misc-error
    (let* [(mod  (make-module))
           (fun  (make-function mod void "incomplete"))]
      (llvm-compile llvm)))
  (test-error "Throw error when attempting to compile again"
    'misc-error
    (let* [(mod  (make-module))
           (fun  (make-function mod void "empty"))]
      (function-ret fun)
      (llvm-compile llvm)
      (llvm-compile llvm)))
  (for-each
    (lambda (type sign bits value)
      (test-equal (format #f "Compile and run function returning a ~a ~a-bit integer" sign bits)
        value
        (let* [(mod (make-module))
               (fun (make-function mod type "constant_int"))]
          (function-ret fun (make-constant type value))
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
         (let* [(mod  (make-module))
                (fun  (make-function mod type "constant_double"))]
           (function-ret fun (make-constant type 0.5))
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
             (mod  (make-module))
             (fun  (make-function mod type "read_mem"))]
        (function-ret fun (function-load fun type (make-constant int64 (pointer-address (bytevector->pointer data)))))
        (llvm-compile mod)
        ((llvm-func mod fun)))))
    '(2 770)
    (list int8 int16)
    '("byte" "short integer"))
  (for-each (lambda (data value type name)
    (test-equal (format #f "Write ~a to memory" name)
      #vu8(2 3 5 7)
      (let* [(mod  (make-module))
             (fun  (make-function mod void "write_mem"))]
        (function-store fun type (make-constant type value) (make-constant int64 (pointer-address (bytevector->pointer data))))
        (function-ret fun)
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
    (let [(mod (make-module))]
      (make-function mod int "with_arg" int)))
  (test-assert "Call a function accepting an argument"
    (let* [(mod  (make-module))
           (fun  (make-function mod void "accept_arg" int))]
      (function-ret fun)
      (llvm-compile mod)
      ((llvm-func mod fun) 42)))
  (test-equal "Compile, verify, and run integer identity function"
    42
    (let* [(mod  (make-module))
           (fun  (make-function mod int "int_identity" int))]
      (function-ret fun (function-param fun 0))
      (llvm-compile mod)
      ((llvm-func mod fun) 42)))
  (test-equal "Compile, verify, and run floating point identity function"
    0.5
    (let* [(mod  (make-module))
           (fun  (make-function mod double "double_identity" double))]
      (function-ret fun (function-param fun 0))
      (llvm-compile mod)
      ((llvm-func mod fun) 0.5)))
(test-end "method arguments")

(test-begin "unary expressions")
  (test-equal "Negate integer"
    -42
    (let* [(mod (make-module))
           (fun (make-function mod int "neg" int))]
      (function-ret fun (llvm-neg fun (function-param fun 0)))
      (llvm-compile mod)
      ((llvm-func mod fun) 42)))
(test-end "unary expressions")

(test-begin "binary expressions")
  (test-equal "Add two integers"
    5
    (let* [(mod (make-module))
           (fun (make-function mod int "add" int int))]
      (function-ret fun (llvm-add fun (function-param fun 0) (function-param fun 1)))
      (llvm-compile mod)
      ((llvm-func mod fun) 2 3)))
  (test-equal "Add two floating-point numbers"
    5.75
    (let* [(mod (make-module))
           (fun (make-function mod double "add" double double))]
      (function-ret fun (llvm-fadd fun (function-param fun 0) (function-param fun 1)))
      (llvm-compile mod)
      ((llvm-func mod fun) 2.5 3.25)))
(test-end "binary expressions")
(test-end "aiscm llvm")
