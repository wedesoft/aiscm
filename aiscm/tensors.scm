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
(define-module (aiscm tensors)
  #:use-module (oop goops)
  #:use-module (aiscm core)
  #:use-module (aiscm util)
  #:export (<tensor> <index> <functional> <lookup> <elementary<>>
            expression->tensor)
  #:export-syntax (define-tensor tensor term index stride elementary)
  #:re-export (get memory shape typecode))

(define-class <tensor> ())

(define-class <index> (<tensor>)
  (size #:init-keyword #:size #:getter size))

(define-class <elementary<>> (<tensor>)
  (memory #:init-keyword #:memory #:getter memory))

(define (elementary type)
  (template-class (elementary type) <elementary<>>
    (lambda (class metaclass)
      (define-method (typecode (self class)) type))))

(define-class <functional> (<tensor>)
  (index #:init-keyword #:index #:getter index)
  (term  #:init-keyword #:term  #:getter term ))

(define-method (shape (self <functional>))
  (attach (shape (term self)) (size (index self))))

(define-method (typecode (self <functional>))
  (typecode (term self)))

(define-class <lookup> (<tensor>)
  (index  #:init-keyword #:index  #:getter index )
  (stride #:init-keyword #:stride #:getter stride)
  (term  #:init-keyword #:term  #:getter term ))

(define-method (typecode (self <lookup>))
  (typecode (term self)))

(define-method (get (self <llvmarray<>>) (idx <index>))
  self)

(define-method (expression->tensor self)
  "Pass-through value by default"
  self)

(define-method (lookup i s t)
  "Instantiate lookup object"
  (make <lookup> #:index i #:stride s #:term t))

(define-method (lookup i s (t <functional>))
  "Swap order so that functional objects are not inside lookup objects"
  (make <functional> #:index (index t) #:term (lookup i s (term t))))

(define-method (expression->tensor (self <llvmarray<>>))
  "Convert compiled array to function of index"
  (if (zero? (dimensions self))
    (make (elementary (typecode self)) #:memory (memory self))
    (let [(i (make <index>))]
      (slot-set! i 'size (llvm-last (shape self)))
      (make <functional>
            #:term (lookup i (llvm-last (strides self)) (expression->tensor (project self)))
            #:index i))))

(define-syntax tensor
  (lambda (x)
    (syntax-case x ()
      ((k i expression) #'(let [(i (make <index>))] expression)))))

(define (adapted-native-type value) (if (is-a? value <integer>) <int> (native-type value)))

(define-syntax-rule (define-tensor (name args ...) expression)
  (define-method (name args ...)
    (let [(fun (jit (map adapted-native-type (list args ...))
                 (lambda (args ...)
                   expression)))]
      (add-method! name
                   (make <method> #:specializers (map class-of (list args ...))
                                  #:procedure fun))
      (name args ...))))
