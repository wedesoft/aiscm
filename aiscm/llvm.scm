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
(define-module (aiscm llvm)
  #:use-module (oop goops)
  #:export (<llvm>
            <function>
            make-llvm
            make-function
            function-ret
            function-compile
            function-apply))

(define-class <llvm> ())
(define-class <function> ())

(define (make-llvm) (make <llvm>))

(define (make-function llvm) (make <function>))

(define (function-ret fun) #f)
(define (function-compile fun) #f)
(define (function-apply fun) (if #f #f))
