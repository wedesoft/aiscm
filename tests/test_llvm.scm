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

(test-begin "LLVM context")
  (test-equal "Create LLVM instance"
    <llvm> (class-of (make-llvm)))
  (test-assert "Destroy LLVM instance"
    (unspecified? (destroy (make-llvm))))
  (test-assert "LLVM context slot defined"
    (slot-ref (make-llvm) 'llvm-context))
(test-end "LLVM context")

(test-begin "LLVM value")
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
(test-end "LLVM value")

(test-begin "LLVM function")
  (test-equal "Create LLVM function"
    <llvm-function> (class-of (let [(llvm (make-llvm))] (make-function llvm void "function"))))
  (let [(llvm (make-llvm))]
    (test-equal "Keep LLVM instance alive"
      llvm (slot-ref (make-function llvm void "function") 'context)))
  (test-assert "Compile, verify, and run empty function"
    (unspecified?
      (let* [(llvm (make-llvm))
             (fun  (make-function llvm void "empty"))]
        (function-ret fun)
        (llvm-verify llvm)
        (llvm-apply llvm fun))))
  (test-assert "Dump module containing a function"
    (unspecified?
      (let* [(llvm (make-llvm))
             (fun  (make-function llvm void "empty"))]
        (function-ret fun)
        (llvm-dump llvm))))
  (test-error "Throw error if verification of module failed"
    'misc-error
    (let* [(llvm (make-llvm))
           (fun  (make-function llvm void "incomplete"))]
      (llvm-verify llvm)
      (llvm-apply llvm fun)))
  (for-each
    (lambda (type sign bits value)
      (test-equal (format #f "Compile and run function returning a ~a ~a-bit integer" sign bits)
        value
        (let* [(llvm (make-llvm))
               (fun  (make-function llvm type "constant_int"))]
          (function-ret fun (make-constant type value))
          (llvm-apply llvm fun))))
    (list int8 int16 int32 int64 uint8 uint16 uint32 uint64)
    (append (make-list 4 "signed") (make-list 4 "unsigned"))
    '(8 16 32 64 8 16 32 64)
    '(-128 -32768 -2147483648 -9223372036854775808 255 65535 4294967295 18446744073709551615))
   (for-each
     (lambda (type precision)
       (test-equal (format #f "Compile and run function returning a ~a-precision floating point number" precision)
          0.5
          (let* [(llvm (make-llvm))
                 (fun  (make-function llvm type "constant_double"))]
            (function-ret fun (make-constant type 0.5))
            (llvm-apply llvm fun))))
     (list float double)
     (list "single" "double"))
(test-end "LLVM function")

(test-begin "LLVM pointer")
  (test-equal "Load unsigned byte from memory"
    42
    (let* [(data #vu8(42 1 2 3))
           (llvm (make-llvm))
           (fun  (make-function llvm int8 "read_mem"))]
      (function-ret fun (function-load fun int8 (make-constant int64 (pointer-address (bytevector->pointer data)))))
      (llvm-apply llvm fun)))
(test-end "LLVM pointer")
(test-end "aiscm llvm")
