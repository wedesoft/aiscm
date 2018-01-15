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
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 curried-definitions)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (aiscm basictype)
  #:use-module (aiscm util)
  #:export (<llvm> <meta<llvm>>
            <llvm-function> <meta<llvm-function>>
            make-constant make-constant-pointer make-llvm-module make-function llvm-dump
            function-ret llvm-func get-type llvm-compile function-load function-store function-param
            llvm-neg llvm-fneg llvm-not llvm-add llvm-fadd llvm-sub llvm-fsub llvm-mul llvm-fmul
            llvm-sequential llvm-wrap llvm-trunc llvm-sext llvm-zext llvm-typed to-type
            llvm-fp-cast llvm-fp-to-si llvm-fp-to-ui llvm-si-to-fp llvm-ui-to-fp
            llvm-call
            ~)
  #:export-syntax (llvm-let*)
  #:re-export (destroy - + *))

(load-extension "libguile-aiscm-llvm" "init_llvm")

(define-class* <llvm> <object> <meta<llvm>> <class>
               (llvm-module #:init-keyword #:llvm-module))

(define-method (initialize (self <llvm>) initargs)
  (next-method self (list #:llvm-module (make-llvm-module-base))))

(define (make-llvm-module) (make <llvm>))

(define-method (destroy (self <llvm>)) (llvm-module-destroy (slot-ref self 'llvm-module)))

(define-class* <llvm-function> <object> <meta<llvm-function>> <class>
               (module         #:init-keyword #:module        )
               (name           #:init-keyword #:name          )
               (return-type    #:init-keyword #:return-type   )
               (llvm-function  #:init-keyword #:llvm-function )
               (argument-types #:init-keyword #:argument-types))

(define-method (initialize (self <llvm-function>) initargs)
  (let-keywords initargs #f (module return-type name argument-types)
    (let* [(fun (make-llvm-function (slot-ref module 'llvm-module)
                                   return-type
                                   name
                                   argument-types))]
    (next-method self (list #:module         module
                            #:name           name
                            #:return-type    return-type
                            #:llvm-function  fun
                            #:argument-types argument-types)))))

(define (make-function module return-type name . argument-types)
  (make <llvm-function> #:module         module
                        #:return-type    return-type
                        #:name           name
                        #:argument-types argument-types))

(define-method (destroy (self <llvm-function>)) (llvm-function-destroy (slot-ref self 'llvm-function)))

(define* ((function-ret #:optional (result (lambda (fun) #f))) fun)
  (let [(llvm-function (slot-ref fun 'llvm-function))
        (return-value  (result fun))]
    (if return-value
      (llvm-function-return llvm-function (car return-value))
      (llvm-function-return-void llvm-function))))

(define (llvm-dump self) (llvm-dump-module (slot-ref self 'llvm-module)))

(define (llvm-compile self)
  (llvm-verify-module (slot-ref self 'llvm-module))
  (llvm-compile-module (slot-ref self 'llvm-module))
  (if (equal? "YES" (getenv "DEBUG")) (llvm-dump self)))

(define (llvm-func llvm fun)
  (let [(pointer (llvm-get-function-address (slot-ref llvm 'llvm-module) (slot-ref fun 'name)))]
    (pointer->procedure (slot-ref fun 'return-type) pointer (slot-ref fun 'argument-types))))

(define ((make-constant type value) fun)
  "Create a constant LLVM value"
  (list (make-llvm-constant type value)))

(define (make-constant-pointer address)
  "Create pointer constant"
  (make-constant int64 (pointer-address address)))

(define (get-type value)
  "Query type of LLVM value"
  (map llvm-get-type value))

(define ((function-load type address) fun)
  "Generate code for reading value from memory"
  (list (llvm-build-load (slot-ref fun 'llvm-function) type (car (address fun)))))

(define ((function-store type value address) fun)
  "Generate code for writing value to memory"
  (llvm-build-store (slot-ref fun 'llvm-function) type (car (value fun)) (car (address fun))))

(define ((function-param index) fun)
  "Get value of INDEXth function parameter"
  (list (llvm-get-param (slot-ref fun 'llvm-function) index)))

(define-syntax-rule (define-llvm-unary function delegate)
  (define ((function value) fun)
    (list (delegate (slot-ref fun 'llvm-function) (car (value fun))))))

(define-llvm-unary llvm-neg  llvm-build-neg )
(define-llvm-unary llvm-fneg llvm-build-fneg)
(define-llvm-unary llvm-not  llvm-build-not )

(define-syntax-rule (define-llvm-binary function delegate)
  (define ((function value-a value-b) fun)
    (list (delegate (slot-ref fun 'llvm-function) (car (value-a fun)) (car (value-b fun))))))

(define-llvm-binary llvm-add  llvm-build-add )
(define-llvm-binary llvm-fadd llvm-build-fadd)
(define-llvm-binary llvm-sub  llvm-build-sub )
(define-llvm-binary llvm-fsub llvm-build-fsub)
(define-llvm-binary llvm-mul  llvm-build-mul )
(define-llvm-binary llvm-fmul llvm-build-fmul)

(define-syntax-rule (define-llvm-cast function delegate)
  (define ((function type value) fun)
    (list (delegate (slot-ref fun 'llvm-function) type (car (value fun))))))

(define-llvm-cast llvm-trunc    llvm-build-trunc   )
(define-llvm-cast llvm-sext     llvm-build-sext    )
(define-llvm-cast llvm-zext     llvm-build-zext    )
(define-llvm-cast llvm-fp-cast  llvm-build-fp-cast )
(define-llvm-cast llvm-fp-to-si llvm-build-fp-to-si)
(define-llvm-cast llvm-fp-to-ui llvm-build-fp-to-ui)
(define-llvm-cast llvm-si-to-fp llvm-build-si-to-fp)
(define-llvm-cast llvm-ui-to-fp llvm-build-ui-to-fp)

(define module-list '())

(define ((llvm-sequential instruction . instructions) fun)
  "Execute list of instructions sequentially"
  (if (null? instructions)
    (instruction fun)
    (begin
      (instruction fun)
      ((apply llvm-sequential instructions) fun))))

(define (llvm-wrap foreign-types function)
  "Convenience wrapper for compiling JIT functions"
  (let* [(mod         (make-llvm-module))
         (arguments   (map function-param (iota (length foreign-types))))
         (result      (apply function arguments))
         (return-type (car result))
         (expression  (cdr result))
         (fun         (apply make-function mod return-type "wrapped" foreign-types)) ]
    (expression fun)
    (llvm-compile mod)
    (set! module-list (cons mod module-list))
    (llvm-func mod fun)))

(define-syntax llvm-let*
  (lambda (x)
    (syntax-case x ()
      ((llvm-let* [] body ...)
       #'(llvm-sequential body ...))
      ((llvm-let* [(variable expression) definitions ...] body ...)
       #'(lambda (fun)
           (let* [(intermediate (expression fun))
                  (variable (lambda (fun) intermediate))]
             ((llvm-let* [definitions ...] body ...) fun)))))))

(define-method (to-type (cls <meta<int<>>>) (value <int<>>))
  "Integer conversions"
  (let [(conversion (if (> (bits cls) (bits value)) (if (signed? value) llvm-sext llvm-zext) llvm-trunc))]
    (make cls #:value (conversion (foreign-type cls) (get value)))))

(define-method (to-type (cls <meta<float<>>>) (value <float<>>))
  "Floating-point conversions"
  (make cls #:value (llvm-fp-cast (foreign-type cls) (get value))))

(define-method (to-type (cls <meta<float<>>>) (value <int<>>))
  "Convert integer to floating-point"
  (let [(conversion (if (signed? value) llvm-si-to-fp llvm-ui-to-fp))]
    (make cls #:value (conversion (foreign-type cls) (get value)))))

(define-method (to-type (cls <meta<int<>>>) (value <float<>>))
  "Floating-point to integer conversion"
  (let [(conversion (if (signed? cls) llvm-fp-to-si llvm-fp-to-ui))]
    (make cls #:value (conversion (foreign-type cls) (get value)))))

(define-syntax-rule (define-unary-operation type operation delegate)
  (define-method (operation (value type))
    (make (class-of value) #:value (delegate (get value)))))

(define-unary-operation <int<>>   - llvm-neg )
(define-unary-operation <float<>> - llvm-fneg)
(define-unary-operation <int<>>   ~ llvm-not )

(define-syntax-rule (define-binary-operation type-a type-b operation delegate)
  (define-method (operation (value-a type-a) (value-b type-b))
    (let* [(target  (coerce (class-of value-a) (class-of value-b)))
           (adapt-a (to-type target value-a ))
           (adapt-b (to-type target value-b))]
      (make target #:value (delegate (get adapt-a) (get adapt-b))))))

(define-binary-operation <int<>>   <int<>>   + llvm-add )
(define-binary-operation <float<>> <int<>>   + llvm-fadd)
(define-binary-operation <int<>>   <float<>> + llvm-fadd)
(define-binary-operation <float<>> <float<>> + llvm-fadd)
(define-binary-operation <int<>>   <int<>>   - llvm-sub )
(define-binary-operation <int<>>   <float<>> - llvm-fsub)
(define-binary-operation <float<>> <int<>>   - llvm-fsub)
(define-binary-operation <float<>> <float<>> - llvm-fsub)
(define-binary-operation <int<>>   <int<>>   * llvm-mul )
(define-binary-operation <int<>>   <float<>> * llvm-fmul)
(define-binary-operation <float<>> <int<>>   * llvm-fmul)
(define-binary-operation <float<>> <float<>> * llvm-fmul)

(define-method (complex (value-a <float<>>) (value-b <float<>>))
  (make (complex (class-of value-a)) #:value (lambda (fun) (append ((get value-a) fun) ((get value-b) fun)))))

(define-method (+ (value-a <complex<>>) (value-b <complex<>>))
  (complex (+ (real-part value-a) (real-part value-b))
           (+ (imag-part value-a) (imag-part value-b))))

(define-method (prepare-return (result <complex<>>) memory)
  (lambda (fun)
    (let* [(base (base (class-of result)))
           (address0 memory)
           (address1 (llvm-add memory (make-constant int64 (size-of base))))
           (lst      ((get result) fun))]
      ((function-store (foreign-type base) (lambda (fun) (list (car  lst))) address0) fun)
      ((function-store (foreign-type base) (lambda (fun) (list (cadr lst))) address1) fun)
      ((function-ret memory) fun))))

(define-method (prepare-return (result <scalar>) memory)
  (function-ret (get result)))

(define-method (finish-return type result)
  (unpack-value type result))

(define-method (finish-return (type <meta<scalar>>) result)
  result)

(define (llvm-typed argument-types function)
  "Infer types and compile function"
  (let* [(result-type #f)
         (fun (llvm-wrap (cons int64 (map foreign-type (decompose-types argument-types)))
               (lambda arguments
                 (let* [(arguments-typed (compose-values argument-types (cdr arguments)))
                        (expression      (apply function arguments-typed))]
                   (set! result-type (class-of expression))
                   (cons (foreign-type result-type) (prepare-return expression (car arguments)))))))]
    (lambda args
      (let [(memory (make-bytevector (size-of result-type)))]
        (finish-return
          result-type
          (apply fun
            (cons (pointer-address (bytevector->pointer memory))
                  (decompose-arguments argument-types args))))))))

(define ((llvm-call return-type function-name argument-types args) fun)
  (list (llvm-build-call (slot-ref fun 'llvm-function)
                         (slot-ref (slot-ref fun 'module) 'llvm-module)
                         return-type
                         function-name
                         argument-types
                         (map (lambda (arg) (car (arg fun))) args))))
