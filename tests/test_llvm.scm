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

(test-equal "Create LLVM instance"
  <llvm> (class-of (make-llvm)))
(test-assert "Destroy LLVM instance"
  (unspecified? (destroy (make-llvm))))
(test-assert "LLVM context slot defined"
  (slot-ref (make-llvm) 'llvm-context))
(test-equal "Create LLVM function"
  <function> (class-of (let [(llvm (make-llvm))] (make-function llvm "test1"))))
(let [(llvm (make-llvm))]
  (test-equal "Keep LLVM instance alive"
    llvm (slot-ref (make-function llvm "test2") 'context)))
(test-assert "Compile and run empty function"
  (unspecified?
    (let* [(llvm (make-llvm))
           (fun  (make-function llvm "test3"))]
      (function-ret fun)
      (llvm-apply llvm fun))))

(test-end "aiscm llvm")
