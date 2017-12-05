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
  #:use-module (ice-9 optargs)
  #:use-module (aiscm util)
  #:export (<llvm> <meta<llvm>>
            <llvm-function> <meta<llvm-function>>
            <llvm-value> <meta<llvm-value>>
            make-constant make-llvm make-function function-ret llvm-apply get-type)
  #:re-export (destroy))

(load-extension "libguile-aiscm-llvm" "init_llvm")

(define-class* <llvm> <object> <meta<llvm>> <class>
               (llvm-context #:init-keyword #:llvm-context))

(define-method (initialize (self <llvm>) initargs)
  (next-method self (list #:llvm-context  (make-llvm-context))))

(define (make-llvm) (make <llvm>))

(define-method (destroy (self <llvm>)) (llvm-context-destroy (slot-ref self 'llvm-context)))

(define-class* <llvm-function> <object> <meta<llvm-function>> <class>
               (context       #:init-keyword #:context      )
               (return-type   #:init-keyword #:return-type  )
               (llvm-function #:init-keyword #:llvm-function))

(define-method (initialize (self <llvm-function>) initargs)
  (let-keywords initargs #f (context return-type name)
    (next-method self (list #:context context
                            #:return-type return-type
                            #:llvm-function (make-llvm-function (slot-ref context 'llvm-context) return-type name)))))

(define (make-function llvm return-type name) (make <llvm-function> #:context llvm #:return-type return-type #:name name))

(define-method (destroy (self <llvm-function>)) (llvm-function-destroy (slot-ref self 'llvm-function)))

(define* (function-ret self #:optional (result #f))
  (let [(llvm-function (slot-ref self 'llvm-function))]
    (if result
      (llvm-function-return llvm-function (slot-ref result 'llvm-value))
      (llvm-function-return-void llvm-function))))

(define (llvm-apply llvm fun)
  (llvm-verify-module (slot-ref llvm 'llvm-context))
  (llvm-context-apply (slot-ref llvm 'llvm-context) (slot-ref fun 'return-type) (slot-ref fun 'llvm-function)))

(define-class* <llvm-value> <object> <meta<llvm-value>> <class>
               (llvm-value #:init-keyword #:llvm-value))

(define-method (initialize (self <llvm-value>) initargs)
  (let-keywords initargs #f (type value)
    (next-method self (list #:llvm-value (make-llvm-constant type value)))))

(define (make-constant type value)
  (make <llvm-value> #:type type #:value value))

(define (get-type value)
  (llvm-get-type (slot-ref value 'llvm-value)))
