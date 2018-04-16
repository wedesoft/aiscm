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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 poe)
  #:use-module (ice-9 curried-definitions)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (aiscm basictype)
  #:use-module (aiscm util)
  #:export (<llvm> <meta<llvm>>
            <llvm-function> <meta<llvm-function>>
            make-constant make-constant-pointer make-llvm-module make-function llvm-dump
            function-ret llvm-func get-type llvm-compile llvm-fetch llvm-store function-param
            llvm-neg llvm-fneg llvm-not llvm-add llvm-fadd llvm-sub llvm-fsub llvm-mul llvm-fmul
            llvm-wrap llvm-trunc llvm-sext llvm-zext llvm-typed to-type return
            llvm-fp-cast llvm-fp-to-si llvm-fp-to-ui llvm-si-to-fp llvm-ui-to-fp
            llvm-sequential llvm-call typed-constant typed-pointer store fetch llvm-begin
            ~)
  #:export-syntax (memoize define-uniform-constructor define-mixed-constructor)
  #:re-export (destroy - + *))


; TODO: move into test suite and integrate into library

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

(define-syntax-rule (memoize (arguments ...) body ...)
  (perfect-funcq 1 (lambda (arguments ...) body ...)))

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

(define (make-constant type value)
  "Create a constant LLVM value"
  (memoize (fun) (list (make-llvm-constant type value))))

(define (make-constant-pointer address)
  "Create pointer constant"
  (make-constant int64 (pointer-address address)))

(define (get-type value)
  "Query type of LLVM value"
  (map llvm-get-type value))

(define (llvm-fetch type address)
  "Generate code for reading value from memory"
  (memoize (fun) (list (llvm-build-load (slot-ref fun 'llvm-function) type (car (address fun))))))

(define ((llvm-store type value address) fun)
  "Generate code for writing value to memory"
  (llvm-build-store (slot-ref fun 'llvm-function) type (car (value fun)) (car (address fun))))

(define ((function-param index) fun)
  "Get value of INDEXth function parameter"
  (list (llvm-get-param (slot-ref fun 'llvm-function) index)))

(define-syntax-rule (define-llvm-unary function delegate)
  (define (function value)
    (memoize (fun) (list (delegate (slot-ref fun 'llvm-function) (car (value fun)))))))

(define-llvm-unary llvm-neg  llvm-build-neg )
(define-llvm-unary llvm-fneg llvm-build-fneg)
(define-llvm-unary llvm-not  llvm-build-not )

(define-syntax-rule (define-llvm-binary function delegate)
  (define (function value-a value-b)
    (memoize (fun) (list (delegate (slot-ref fun 'llvm-function) (car (value-a fun)) (car (value-b fun)))))))

(define-llvm-binary llvm-add  llvm-build-add )
(define-llvm-binary llvm-fadd llvm-build-fadd)
(define-llvm-binary llvm-sub  llvm-build-sub )
(define-llvm-binary llvm-fsub llvm-build-fsub)
(define-llvm-binary llvm-mul  llvm-build-mul )
(define-llvm-binary llvm-fmul llvm-build-fmul)

(define-syntax-rule (define-llvm-cast function delegate)
  (define (function type value)
    (memoize (fun) (list (delegate (slot-ref fun 'llvm-function) type (car (value fun)))))))

(define-llvm-cast llvm-trunc    llvm-build-trunc   )
(define-llvm-cast llvm-sext     llvm-build-sext    )
(define-llvm-cast llvm-zext     llvm-build-zext    )
(define-llvm-cast llvm-fp-cast  llvm-build-fp-cast )
(define-llvm-cast llvm-fp-to-si llvm-build-fp-to-si)
(define-llvm-cast llvm-fp-to-ui llvm-build-fp-to-ui)
(define-llvm-cast llvm-si-to-fp llvm-build-si-to-fp)
(define-llvm-cast llvm-ui-to-fp llvm-build-ui-to-fp)

(define module-list '())

(define (return . args)
  (make <void> #:value (apply function-ret (map get args))))

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

(define-syntax-rule (define-op-with-constant type operation)
  (begin
    (define-method (operation (value-a type) (value-b <complex>))
      (operation value-a (typed-constant (native-type value-b) value-b)))
    (define-method (operation (value-a <complex>) (value-b type))
      (operation (typed-constant (native-type value-a) value-a) value-b))))

(define-syntax-rule (define-binary-delegation operation delegate float-delegate)
  (begin
    (define-binary-operation <int<>>   <int<>>   operation delegate )
    (define-binary-operation <float<>> <int<>>   operation float-delegate)
    (define-binary-operation <int<>>   <float<>> operation float-delegate)
    (define-binary-operation <float<>> <float<>> operation float-delegate)
    (define-op-with-constant <void> operation)))

(define-binary-delegation + llvm-add llvm-fadd)
(define-binary-delegation - llvm-sub llvm-fsub)
(define-binary-delegation * llvm-mul llvm-fmul)

(define (construct-object class args)
  (make class #:value (lambda (fun) (append-map (lambda (component) ((get component) fun)) args))))

(define-syntax-rule (define-mixed-constructor name)
  (define-method (name . args)
    (construct-object (apply name (map class-of args)) args)))

(define-syntax-rule (define-uniform-constructor name)
  (define-method (name . args)
    (let* [(target  (reduce coerce #f (map class-of args)))
           (adapted (map (cut to-type target <>) args))]
      (construct-object (name target) adapted))))

(define-uniform-constructor complex)

(define-method (- (value <complex<>>))
  (complex (- (real-part value)) (- (imag-part value))))

(define-method (+ (value-a <complex<>>) (value-b <complex<>>))
  (complex (+ (real-part value-a) (real-part value-b)) (+ (imag-part value-a) (imag-part value-b))))

(define-method (+ (value-a <complex<>>) (value-b <scalar>))
  (complex (+ (real-part value-a) value-b) (imag-part value-a)))

(define-method (+ (value-a <scalar>) (value-b <complex<>>))
  (complex (+ value-a (real-part value-b)) (imag-part value-b)))

(define-method (- (value-a <complex<>>) (value-b <complex<>>))
  (complex (- (real-part value-a) (real-part value-b)) (- (imag-part value-a) (imag-part value-b))))

(define-method (- (value-a <complex<>>) (value-b <scalar>))
  (complex (- (real-part value-a) value-b) (imag-part value-a)))

(define-method (- (value-a <scalar>) (value-b <complex<>>))
  (complex (- value-a (real-part value-b)) (- (imag-part value-b))))

(define (llvm-begin instruction . instructions)
  (if (null? instructions)
    instruction
    (let [(result (apply llvm-begin instructions))]
      (make (class-of result) #:value (lambda (fun) ((get instruction) fun) ((get result) fun))))))

(define-method (typed-constant (type <meta<scalar>>) value)
  (make type #:value (make-constant (foreign-type type) value)))

(define-method (typed-constant (type <meta<void>>) value)
  (apply (build type)
         (map (lambda (type component) (typed-constant type (component value)))
              (base type)
              (components type))))

(define (typed-pointer value)
  (make <long> #:value (make-constant-pointer value)))

(define-method (store address (value <scalar>))
  (make <void> #:value (llvm-store (foreign-type (class-of value)) (get value) (get address))))

(define-method (store address (value <void>))
  (apply llvm-begin
    (map (lambda (component offset) (store (+ address offset) (component value)))
         (components (class-of value))
         (integral (cons 0 (all-but-last (map size-of (base (class-of value)))))))))

(define-method (fetch (type <meta<scalar>>) address)
  (make type #:value (llvm-fetch (foreign-type type) (get address))))

(define-method (fetch (type <meta<void>>) address)
  (apply (build type) (map (lambda (type offset) (fetch type (+ address offset)))
                           (base type)
                           (integral (cons 0 (all-but-last (map size-of (base type))))))))

(define-method (prepare-return (result <void>) memory)
  (if (null? (components (class-of result)))
    (llvm-begin result (return))
    (llvm-begin (store memory result) (return memory))))

(define-method (prepare-return (result <scalar>) memory)
  (return result))

(define-method (finish-return type result)
  (unpack-value type result))

(define-method (finish-return (type <meta<scalar>>) result)
  result)

(define (llvm-typed argument-types function)
  "Infer types and compile function"
  (let* [(result-type #f)
         (fun (llvm-wrap (cons int64 (map foreign-type (decompose-types argument-types)))
               (lambda arguments
                 (let* [(arguments-typed (compose-values (cons <long> argument-types) arguments))
                        (expression      (apply function (cdr arguments-typed)))]
                   (set! result-type (class-of expression))
                   (cons (foreign-type result-type) (get (prepare-return expression (car arguments-typed))))))))]
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
