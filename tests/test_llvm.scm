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
  (test-equal "Return type of 32-bit integer value"
    int32 (get-type (make-constant int32 42)))
  (test-equal "Type of value ignores signed-ness"
    int32 (get-type (make-constant uint32 42)))
(test-end "LLVM value")

(test-begin "LLVM function")
  (test-equal "Create LLVM function"
    <llvm-function> (class-of (let [(llvm (make-llvm))] (make-function llvm void "test1"))))
  (let [(llvm (make-llvm))]
    (test-equal "Keep LLVM instance alive"
      llvm (slot-ref (make-function llvm void "test2") 'context)))
  (test-assert "Compile and run empty function"
    (unspecified?
      (let* [(llvm (make-llvm))
             (fun  (make-function llvm void "test3"))]
        (function-ret fun)
        (llvm-apply llvm fun))))
  (test-error "Throw error if verification of module failed"
    'misc-error
    (let* [(llvm (make-llvm))
           (fun  (make-function llvm void "test4"))]
      (llvm-apply llvm fun)))
  (test-equal "Compile and run function returning an integer"
    -42
    (let* [(llvm (make-llvm))
           (fun  (make-function llvm int32 "test5"))]
      (function-ret fun (make-constant int32 -42))
      (llvm-apply llvm fun)))
  (test-equal "Compile and run function returning an unsigned integer"
    42
    (let* [(llvm (make-llvm))
           (fun  (make-function llvm uint32 "test5"))]
      (function-ret fun (make-constant uint32 42))
      (llvm-apply llvm fun)))
(test-end "LLVM function")

(test-end "aiscm llvm")
