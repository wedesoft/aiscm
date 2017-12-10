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
    <llvm-function> (class-of (let [(llvm (make-module))] (make-function llvm void "function"))))
  (let [(llvm (make-module))]
    (test-equal "Keep LLVM instance alive"
      llvm (slot-ref (make-function llvm void "function") 'module)))
  (test-assert "Compile, verify, and run empty function"
    (unspecified?
      (let* [(llvm (make-module))
             (fun  (make-function llvm void "empty"))]
        (function-ret fun)
        (llvm-verify llvm)
        (llvm-apply llvm fun))))
  (test-assert "Dump module containing a function"
    (unspecified?
      (let* [(llvm (make-module))
             (fun  (make-function llvm void "empty"))]
        (function-ret fun)
        (llvm-dump llvm))))
  (test-error "Throw error if verification of module failed"
    'misc-error
    (let* [(llvm (make-module))
           (fun  (make-function llvm void "incomplete"))]
      (llvm-verify llvm)
      (llvm-apply llvm fun)))
  (for-each
    (lambda (type sign bits value)
      (test-equal (format #f "Compile and run function returning a ~a ~a-bit integer" sign bits)
        value
        (let* [(llvm (make-module))
               (fun  (make-function llvm type "constant_int"))]
          (function-ret fun (make-constant type value))
          (llvm-verify llvm)
          (llvm-apply llvm fun))))
    (list int8 int16 int32 int64 uint8 uint16 uint32 uint64)
    (append (make-list 4 "signed") (make-list 4 "unsigned"))
    '(8 16 32 64 8 16 32 64)
    '(-128 -32768 -2147483648 -9223372036854775808 255 65535 4294967295 18446744073709551615))
  (for-each
    (lambda (type precision)
      (test-equal (format #f "Compile and run function returning a ~a-precision floating point number" precision)
         0.5
         (let* [(llvm (make-module))
                (fun  (make-function llvm type "constant_double"))]
           (function-ret fun (make-constant type 0.5))
           (llvm-verify llvm)
           (llvm-apply llvm fun))))
    (list float double)
    (list "single" "double"))
(test-end "functions")

(test-begin "pointers")
  (for-each (lambda (value type name)
    (test-equal (format #f "Read ~a value from memory" name)
      value
      (let* [(data #vu8(2 3 5 7))
             (llvm (make-module))
             (fun  (make-function llvm type "read_mem"))]
        (function-ret fun (function-load fun type (make-constant int64 (pointer-address (bytevector->pointer data)))))
        (llvm-verify llvm)
        (llvm-apply llvm fun))))
    '(2 770)
    (list int8 int16)
    '("byte" "short integer"))
  (for-each (lambda (data value type name)
    (test-equal (format #f "Write ~a to memory" name)
      #vu8(2 3 5 7)
      (let* [(llvm (make-module))
             (fun  (make-function llvm void "write_mem"))]
        (function-store fun type (make-constant type value) (make-constant int64 (pointer-address (bytevector->pointer data))))
        (function-ret fun)
        (llvm-verify llvm)
        (llvm-apply llvm fun)
        data)))
    (list #vu8(0 3 5 7) #vu8(0 0 5 7))
    '(2 770)
    (list int8 int16)
    '("byte" "short integer"))
(test-end "pointers")

(test-begin "method arguments")
  (test-assert "Declare a function which accepts arguments"
    (let [(llvm (make-module))]
      (make-function llvm int "with_arg" int)))
  (test-assert "Call a function accepting an argument"
    (let* [(llvm (make-module))
           (fun  (make-function llvm void "with_arg" int))]
      (function-ret fun)
      (llvm-verify llvm)
      (llvm-apply llvm fun 42)))
  (test-equal "Compile, verify, and run integer identity function"
    42
    (let* [(llvm (make-module))
           (fun  (make-function llvm int "with_arg" int))]
      (function-ret fun (function-param fun 0))
      (llvm-verify llvm)
      (llvm-apply llvm fun 42)))
  (test-equal "Compile, verify, and run floating point identity function"
    0.5
    (let* [(llvm (make-module))
           (fun  (make-function llvm double "with_arg" double))]
      (function-ret fun (function-param fun 0))
      (llvm-verify llvm)
      (llvm-apply llvm fun 0.5)))
(test-end "method arguments")
(test-end "aiscm llvm")
