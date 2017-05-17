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
(define-module (aiscm jit)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 binary-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (aiscm util)
  #:use-module (aiscm asm)
  #:use-module (aiscm element)
  #:use-module (aiscm scalar)
  #:use-module (aiscm pointer)
  #:use-module (aiscm bool)
  #:use-module (aiscm int)
  #:use-module (aiscm float)
  #:use-module (aiscm obj)
  #:use-module (aiscm composite)
  #:use-module (aiscm sequence)
  #:use-module (aiscm variable)
  #:use-module (aiscm command)
  #:use-module (aiscm program)
  #:use-module (aiscm register-allocate)
  #:use-module (aiscm expression)
  #:use-module (aiscm loop)
  #:use-module (aiscm compile)
  #:use-module (aiscm method)
  #:export (virtual-variables
            code convert-type assemble build-list package-return-content
            content-vars jit operand insert-intermediate
            is-pointer? need-conversion? code-needs-intermediate? call-needs-intermediate?
            force-parameters
            ensure-default-strides unary-extract mutating-code functional-code decompose-value
            decompose-arg delegate-fun generate-return-code
            make-native-function native-call make-constant-function native-const
            scm-eol scm-cons scm-gc-malloc-pointerless scm-gc-malloc operations)
  #:re-export (min max to-type + - && || ! != ~ & | ^ << >> % =0 !=0 conj)
  #:export-syntax (define-jit-method define-operator-mapping pass-parameters))

(define ctx (make <context>))


(define* (virtual-variables results parameters instructions #:key (registers default-registers))
  (jit-compile (flatten-code (relabel (filter-blocks instructions)))
               #:registers registers
               #:parameters parameters
               #:results results
               #:blocked (blocked-intervals instructions)))

(define-method (size-of (self <param>))
  (apply * (native-const <long> (size-of (typecode (type self)))) (shape self)))

(define-method (operand (a <element>)) (get a))
(define-method (operand (a <pointer<>>))
  (if (pointer-offset a)
      (ptr (typecode a) (get a) (pointer-offset a))
      (ptr (typecode a) (get a))))
(define-method (operand (a <param>)) (operand (delegate a)))

(define (insert-intermediate value intermediate fun)
  (append (code intermediate value) (fun intermediate)))

(define-method (code (a <element>) (b <element>)) ((to-type (typecode a) (typecode b)) (parameter a) (list (parameter b))))
(define-method (code (a <element>) (b <integer>)) (list (MOV (operand a) b)))

(define-method (code (a <pointer<>>) (b <pointer<>>))
  (insert-intermediate b (skeleton (typecode a)) (cut code a <>)))
(define-method (code (a <param>) (b <param>)) (code (delegate a) (delegate b)))
(define-method (code (a <indexer>) (b <param>))
  (let [(dest   (multi-loop a))
        (source (multi-loop b))]
    (append (append-map loop-setup (loop-details dest))
            (append-map loop-setup (loop-details source))
            (repeat 0
                    (value (dimension a))
                    (code (body dest) (body source))
                    (append-map loop-increment (loop-details dest))
                    (append-map loop-increment (loop-details source))))))
(define-method (code (out <element>) (fun <function>))
  (if (need-conversion? (typecode out) (type fun))
    (insert-intermediate fun (skeleton (type fun)) (cut code out <>))
    ((term fun) (parameter out))))
(define-method (code (out <pointer<>>) (fun <function>))
  (insert-intermediate fun (skeleton (typecode out)) (cut code out <>)))
(define-method (code (out <param>) (fun <function>)) (code (delegate out) fun))
(define-method (code (out <param>) (value <integer>)) (code out (native-const (type out) value)))

(define (is-pointer? value) (and (delegate value) (is-a? (delegate value) <pointer<>>)))
(define-method (need-conversion? target type) (not (eq? target type)))
(define-method (need-conversion? (target <meta<int<>>>) (type <meta<int<>>>))
  (not (eqv? (size-of target) (size-of type))))
(define-method (need-conversion? (target <meta<bool>>) (type <meta<int<>>>))
  (not (eqv? (size-of target) (size-of type))))
(define-method (need-conversion? (target <meta<int<>>>) (type <meta<bool>>))
  (not (eqv? (size-of target) (size-of type))))
(define (code-needs-intermediate? t value) (or (is-a? value <function>) (need-conversion? t (type value))))
(define (call-needs-intermediate? t value) (or (is-pointer? value) (code-needs-intermediate? t value)))
(define-method (force-parameters (targets <list>) args predicate fun)
  (let* [(mask          (map predicate targets args))
         (intermediates (map-select mask (compose parameter car list) (compose cadr list) targets args))
         (preamble      (concatenate (map-select mask code (const '()) intermediates args)))]
    (attach preamble (apply fun intermediates))))
(define-method (force-parameters target args predicate fun)
  (force-parameters (make-list (length args) target) args predicate fun))

(define (operation-code target op out args)
  "Adapter for nested expressions"
  (force-parameters target args code-needs-intermediate?
    (lambda intermediates
      (apply op (operand out) (map operand intermediates)))))
(define ((functional-code op) out args)
  "Adapter for machine code without side effects on its arguments"
  (operation-code (reduce coerce #f (map type args)) op out args))
(define ((mutating-code op) out args)
  "Adapter for machine code overwriting its first argument"
  (insert-intermediate (car args) out (cut operation-code (type out) op <> (cdr args))))
(define ((unary-extract op) out args)
  "Adapter for machine code to extract part of a composite value"
  (code (delegate out) (apply op (map delegate args))))

(define-macro (define-operator-mapping name arity type fun)
  (let [(header (typed-header (symbol-list arity) type))]
    `(define-method (,name ,@header) ,fun)))

(define-operator-mapping -   1 <meta<int<>>> (mutating-code   NEG              ))
(define-method (- (z <integer>) (a <meta<int<>>>)) (mutating-code NEG))
(define-operator-mapping ~   1 <meta<int<>>> (mutating-code   NOT              ))
(define-operator-mapping =0  1 <meta<int<>>> (functional-code test-zero        ))
(define-operator-mapping !=0 1 <meta<int<>>> (functional-code test-non-zero    ))
(define-operator-mapping !   1 <meta<bool>>  (functional-code test-zero        ))
(define-operator-mapping +   2 <meta<int<>>> (mutating-code   ADD              ))
(define-operator-mapping -   2 <meta<int<>>> (mutating-code   SUB              ))
(define-operator-mapping *   2 <meta<int<>>> (mutating-code   IMUL             ))
(define-operator-mapping /   2 <meta<int<>>> (functional-code div              ))
(define-operator-mapping %   2 <meta<int<>>> (functional-code mod              ))
(define-operator-mapping <<  2 <meta<int<>>> (mutating-code   shl              ))
(define-operator-mapping >>  2 <meta<int<>>> (mutating-code   shr              ))
(define-operator-mapping &   2 <meta<int<>>> (mutating-code   AND              ))
(define-operator-mapping |   2 <meta<int<>>> (mutating-code   OR               ))
(define-operator-mapping ^   2 <meta<int<>>> (mutating-code   XOR              ))
(define-operator-mapping &&  2 <meta<bool>>  (mutating-code   bool-and         ))
(define-operator-mapping ||  2 <meta<bool>>  (mutating-code   bool-or          ))
(define-operator-mapping =   2 <meta<int<>>> (functional-code cmp-equal        ))
(define-operator-mapping !=  2 <meta<int<>>> (functional-code cmp-not-equal    ))
(define-operator-mapping <   2 <meta<int<>>> (functional-code cmp-lower-than   ))
(define-operator-mapping <=  2 <meta<int<>>> (functional-code cmp-lower-equal  ))
(define-operator-mapping >   2 <meta<int<>>> (functional-code cmp-greater-than ))
(define-operator-mapping >=  2 <meta<int<>>> (functional-code cmp-greater-equal))
(define-operator-mapping min 2 <meta<int<>>> (functional-code minor            ))
(define-operator-mapping max 2 <meta<int<>>> (functional-code major            ))

(define-operator-mapping -   1 <meta<element>> (native-fun obj-negate    ))
(define-method (- (z <integer>) (a <meta<element>>)) (native-fun obj-negate))
(define-operator-mapping ~   1 <meta<element>> (native-fun scm-lognot    ))
(define-operator-mapping =0  1 <meta<element>> (native-fun obj-zero-p    ))
(define-operator-mapping !=0 1 <meta<element>> (native-fun obj-nonzero-p ))
(define-operator-mapping !   1 <meta<element>> (native-fun obj-not       ))
(define-operator-mapping +   2 <meta<element>> (native-fun scm-sum       ))
(define-operator-mapping -   2 <meta<element>> (native-fun scm-difference))
(define-operator-mapping *   2 <meta<element>> (native-fun scm-product   ))
(define-operator-mapping /   2 <meta<element>> (native-fun scm-divide    ))
(define-operator-mapping %   2 <meta<element>> (native-fun scm-remainder ))
(define-operator-mapping <<  2 <meta<element>> (native-fun scm-ash       ))
(define-operator-mapping >>  2 <meta<element>> (native-fun obj-shr       ))
(define-operator-mapping &   2 <meta<element>> (native-fun scm-logand    ))
(define-operator-mapping |   2 <meta<element>> (native-fun scm-logior    ))
(define-operator-mapping ^   2 <meta<element>> (native-fun scm-logxor    ))
(define-operator-mapping &&  2 <meta<element>> (native-fun obj-and       ))
(define-operator-mapping ||  2 <meta<element>> (native-fun obj-or        ))
(define-operator-mapping =   2 <meta<element>> (native-fun obj-equal-p   ))
(define-operator-mapping !=  2 <meta<element>> (native-fun obj-nequal-p  ))
(define-operator-mapping <   2 <meta<element>> (native-fun obj-less-p    ))
(define-operator-mapping <=  2 <meta<element>> (native-fun obj-leq-p     ))
(define-operator-mapping >   2 <meta<element>> (native-fun obj-gr-p      ))
(define-operator-mapping >=  2 <meta<element>> (native-fun obj-geq-p     ))
(define-operator-mapping min 2 <meta<element>> (native-fun scm-min       ))
(define-operator-mapping max 2 <meta<element>> (native-fun scm-max       ))

(define-method (decompose-value (target <meta<scalar>>) self) self)

(define-method (delegate-op (target <meta<scalar>>) (intermediate <meta<scalar>>) name out args)
  ((apply name (map type args)) out args))
(define-method (delegate-op (target <meta<sequence<>>>) (intermediate <meta<sequence<>>>) name out args)
  ((apply name (map type args)) out args))
(define-method (delegate-op target intermediate name out args)
  (let [(result (apply name (map (lambda (arg) (decompose-value (type arg) arg)) args)))]
    (append-map code (content (type out) out) (content (type result) result))))
(define (delegate-fun name)
  (lambda (out args) (delegate-op (type out) (reduce coerce #f (map type args)) name out args)))

(define-method (type (self <function>))
  (apply (coercion self) (map type (delegate self))))

(define-macro (n-ary-base name arity coercion fun)
  (let* [(args   (symbol-list arity))
         (header (typed-header args '<param>))]
    `(define-method (,name ,@header) (make-function ,name ,coercion ,fun (list ,@args)))))

(define (content-vars args) (map get (append-map content (map class-of args) args)))

(define (assemble return-args args instructions)
  "Determine result variables, argument variables, and instructions"
  (list (content-vars return-args) (content-vars args) (attach instructions (RET))))

(define (build-list . args)
  "Generate code to package ARGS in a Scheme list"
  (fold-right scm-cons scm-eol args))

(define (package-return-content value)
  "Generate code to package parameter VALUE in a Scheme list"
  (apply build-list (content (type value) value)))

(define-method (construct-value result-type retval expr) '())
(define-method (construct-value (result-type <meta<sequence<>>>) retval expr)
  (let [(malloc (if (pointerless? result-type) scm-gc-malloc-pointerless scm-gc-malloc))]
    (append (append-map code (shape retval) (shape expr))
            (code (last (content result-type retval)) (malloc (size-of retval)))
            (append-map code (strides retval) (default-strides (shape retval))))))

(define (generate-return-code args intermediate expr)
  (let [(retval (skeleton <obj>))]
    (list (list retval)
          args
          (append (construct-value (type intermediate) intermediate expr)
                  (code intermediate expr)
                  (code (parameter retval) (package-return-content intermediate))))))

(define (jit context classes proc)
  (let* [(args         (map skeleton classes))
         (parameters   (map parameter args))
         (expr         (apply proc parameters))
         (result-type  (type expr))
         (intermediate (parameter result-type))
         (result       (generate-return-code args intermediate expr))
         (instructions (asm context
                            <ulong>
                            (map typecode (content-vars args))
                            (apply virtual-variables (apply assemble result))))
         (fun          (lambda header (apply instructions (append-map unbuild classes header))))]
    (lambda args (build result-type (address->scm (apply fun args))))))

(define-macro (define-jit-dispatch name arity delegate)
  (let* [(args   (symbol-list arity))
         (header (typed-header args '<element>))]
    `(define-method (,name ,@header)
       (let [(f (jit ctx (map class-of (list ,@args)) ,delegate))]
         (add-method! ,name
                      (make <method>
                            #:specializers (map class-of (list ,@args))
                            #:procedure (lambda args (apply f (map get args))))))
       (,name ,@args))))

(define-macro (define-nary-collect name arity)
  (let* [(args   (symbol-list arity))
         (header (cons (list (car args) '<element>) (cdr args)))]; TODO: extract and test
    (cons 'begin
          (map
            (lambda (i)
              `(define-method (,name ,@(cycle-times header i))
                (apply ,name (map wrap (list ,@(cycle-times args i))))))
            (iota arity)))))

(define operations '())

(define-syntax-rule (define-jit-method coercion name arity)
  (begin (set! operations (cons (quote name) operations))
         (n-ary-base name arity coercion (delegate-fun name))
         (define-nary-collect name arity)
         (define-jit-dispatch name arity name)))

; various type class conversions
(define-method (convert-type (target <meta<element>>) (self <meta<element>>)) target)
(define-method (convert-type (target <meta<element>>) (self <meta<sequence<>>>)) (multiarray target (dimensions self)))
(define-method (to-bool a) (convert-type <bool> a))
(define-method (to-bool a b) (coerce (to-bool a) (to-bool b)))

; define unary and binary operations
(define-method (+ (a <param>  )) a)
(define-method (+ (a <element>)) a)
(define-method (* (a <param>  )) a)
(define-method (* (a <element>)) a)
(define-jit-dispatch duplicate 1 identity)
(define-jit-method identity -   1)
(define-jit-method identity ~   1)
(define-jit-method to-bool  =0  1)
(define-jit-method to-bool  !=0 1)
(define-jit-method to-bool  !   1)
(define-jit-method coerce   +   2)
(define-jit-method coerce   -   2)
(define-jit-method coerce   *   2)
(define-jit-method coerce   /   2)
(define-jit-method coerce   %   2)
(define-jit-method coerce   <<  2)
(define-jit-method coerce   >>  2)
(define-jit-method coerce   &   2)
(define-jit-method coerce   |   2)
(define-jit-method coerce   ^   2)
(define-jit-method coerce   &&  2)
(define-jit-method coerce   ||  2)
(define-jit-method to-bool  =   2)
(define-jit-method to-bool  !=  2)
(define-jit-method to-bool  <   2)
(define-jit-method to-bool  <=  2)
(define-jit-method to-bool  >   2)
(define-jit-method to-bool  >=  2)
(define-jit-method coerce   min 2)
(define-jit-method coerce   max 2)

(define-method (to-type (target <meta<ubyte>>) (source <meta<obj>>  )) (native-fun scm-to-uint8   ))
(define-method (to-type (target <meta<byte>> ) (source <meta<obj>>  )) (native-fun scm-to-int8    ))
(define-method (to-type (target <meta<usint>>) (source <meta<obj>>  )) (native-fun scm-to-uint16  ))
(define-method (to-type (target <meta<sint>> ) (source <meta<obj>>  )) (native-fun scm-to-int16   ))
(define-method (to-type (target <meta<uint>> ) (source <meta<obj>>  )) (native-fun scm-to-uint32  ))
(define-method (to-type (target <meta<int>>  ) (source <meta<obj>>  )) (native-fun scm-to-int32   ))
(define-method (to-type (target <meta<ulong>>) (source <meta<obj>>  )) (native-fun scm-to-uint64  ))
(define-method (to-type (target <meta<long>> ) (source <meta<obj>>  )) (native-fun scm-to-int64   ))
(define-method (to-type (target <meta<int<>>>) (source <meta<int<>>>)) (functional-code mov       ))
(define-method (to-type (target <meta<int<>>>) (source <meta<bool>> )) (functional-code mov       ))
(define-method (to-type (target <meta<bool>> ) (source <meta<bool>> )) (functional-code mov       ))
(define-method (to-type (target <meta<bool>> ) (source <meta<int<>>>)) (functional-code mov       ))
(define-method (to-type (target <meta<bool>> ) (source <meta<obj>>  )) (native-fun scm-to-bool    ))
(define-method (to-type (target <meta<obj>>  ) (source <meta<obj>>  )) (functional-code mov       ))
(define-method (to-type (target <meta<obj>>  ) (source <meta<ubyte>>)) (native-fun scm-from-uint8 ))
(define-method (to-type (target <meta<obj>>  ) (source <meta<byte>> )) (native-fun scm-from-int8  ))
(define-method (to-type (target <meta<obj>>  ) (source <meta<usint>>)) (native-fun scm-from-uint16))
(define-method (to-type (target <meta<obj>>  ) (source <meta<sint>> )) (native-fun scm-from-int16 ))
(define-method (to-type (target <meta<obj>>  ) (source <meta<uint>> )) (native-fun scm-from-uint32))
(define-method (to-type (target <meta<obj>>  ) (source <meta<int>>  )) (native-fun scm-from-int32 ))
(define-method (to-type (target <meta<obj>>  ) (source <meta<ulong>>)) (native-fun scm-from-uint64))
(define-method (to-type (target <meta<obj>>  ) (source <meta<long>> )) (native-fun scm-from-int64 ))
(define-method (to-type (target <meta<obj>>  ) (source <meta<bool>> )) (native-fun obj-from-bool  ))
(define-method (to-type (target <meta<composite>>) (source <meta<composite>>))
  (lambda (out args)
    (append-map
      (lambda (channel) (code (channel (delegate out)) (channel (delegate (car args)))))
      (components source))))

(define-method (to-type (target <meta<element>>) (a <param>))
  (let [(to-target  (cut to-type target <>))
        (coercion   (cut convert-type target <>))]
    (make-function to-target coercion (delegate-fun to-target) (list a))))
(define-method (to-type (target <meta<element>>) (self <element>))
  (let [(f (jit ctx (list (class-of self)) (cut to-type target <>)))]
    (add-method! to-type
                 (make <method>
                       #:specializers (map class-of (list target self))
                       #:procedure (lambda (target self) (f (get self)))))
    (to-type target self)))

(define (ensure-default-strides img)
  "Create a duplicate of the array unless it is compact"
  (if (equal? (strides img) (default-strides (shape img))) img (duplicate img)))

(define-syntax-rule (pass-parameters parameters body ...)
  (let [(first-six-parameters (take-up-to parameters 6))
        (remaining-parameters (drop-up-to parameters 6))]
    (append (map (lambda (register parameter)
                   (MOV (to-type (native-equivalent (type parameter)) register) (get (delegate parameter))))
                 parameter-registers
                 first-six-parameters)
            (map (lambda (parameter) (PUSH (get (delegate parameter)))) remaining-parameters)
            (list body ...)
            (list (ADD RSP (* 8 (length remaining-parameters)))))))

(define* ((native-fun native) out args)
  (force-parameters (argument-types native) args call-needs-intermediate?
    (lambda intermediates
      (blocked caller-saved
        (pass-parameters intermediates
          (MOV RAX (function-pointer native))
          (CALL RAX)
          (MOV (get (delegate out)) (to-type (native-equivalent (return-type native)) RAX)))))))

(define (make-native-function native . args)
  (make-function make-native-function (const (return-type native)) (native-fun native) args))

(define (native-call return-type argument-types function-pointer)
  (cut make-native-function (make-native-method return-type argument-types function-pointer) <...>))

(define* ((native-data native) out args) (list (MOV (get (delegate out)) (get native))))

(define (make-constant-function native . args) (make-function make-constant-function (const (return-type native)) (native-data native) args))

(define (native-const type value) (make-constant-function (native-value type value)))

; Scheme list manipulation
(define main (dynamic-link))
(define scm-eol (native-const <obj> (scm->address '())))
(define scm-cons (native-call <obj> (list <obj> <obj>) (dynamic-func "scm_cons" main)))
(define scm-gc-malloc-pointerless (native-call <ulong> (list <ulong>) (dynamic-func "scm_gc_malloc_pointerless" main)))
(define scm-gc-malloc             (native-call <ulong> (list <ulong>) (dynamic-func "scm_gc_malloc"             main)))
