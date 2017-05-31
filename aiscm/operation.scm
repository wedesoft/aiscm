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
(define-module (aiscm operation)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 curried-definitions)
  #:use-module (aiscm element)
  #:use-module (aiscm scalar)
  #:use-module (aiscm bool)
  #:use-module (aiscm int)
  #:use-module (aiscm pointer)
  #:use-module (aiscm sequence)
  #:use-module (aiscm asm)
  #:use-module (aiscm command)
  #:use-module (aiscm expression)
  #:use-module (aiscm loop)
  #:use-module (aiscm method)
  #:use-module (aiscm util)
  #:export (make-constant-function native-const need-conversion? code
            code-needs-intermediate? operand code force-parameters
            operation-code mutating-code functional-code unary-extract
            convert-type coerce-where-args
            -= ~= += *= <<= >>= &= |= ^= &&= ||= min= max=
            where)
  #:re-export (size-of min max + - && || ! != ~ & | ^ << >> % =0 !=0)
  #:export-syntax (define-operator-mapping let-skeleton let-parameter))

(define* ((native-data native) out args)
  (list (MOV (get (delegate out)) (get native))))
(define (make-constant-function native . args)
  (make-function make-constant-function (const (return-type native)) (native-data native) args))
(define (native-const type value)
  (make-constant-function (native-value type value)))

(define-method (operand (a <element>)) (get a))
(define-method (operand (a <pointer<>>))
  (if (pointer-offset a)
      (ptr (typecode a) (get a) (pointer-offset a))
      (ptr (typecode a) (get a))))
(define-method (operand (a <param>)) (operand (delegate a)))

(define-method (need-conversion? target type) (not (eq? target type)))
(define-method (need-conversion? (target <meta<int<>>>) (type <meta<int<>>>))
  (not (eqv? (size-of target) (size-of type))))
(define-method (need-conversion? (target <meta<bool>>) (type <meta<int<>>>))
  (not (eqv? (size-of target) (size-of type))))
(define-method (need-conversion? (target <meta<int<>>>) (type <meta<bool>>))
  (not (eqv? (size-of target) (size-of type))))

(define-method (force-parameters (targets <list>) args predicate fun)
  (let* [(mask          (map predicate targets args))
         (intermediates (map-select mask (compose parameter car list) (compose cadr list) targets args))
         (preamble      (concatenate (map-select mask code (const '()) intermediates args)))]
    (attach preamble (apply fun intermediates))))
(define-method (force-parameters target args predicate fun)
  (force-parameters (make-list (length args) target) args predicate fun))

(define-syntax-rule (let-prototype [(name prototype value)] body ...)
  (let [(name prototype)] (list (code name value) body ...)))
(define-syntax-rule (let-skeleton [(name type value)] body ...)
  (let-prototype [(name (skeleton type) value)] body ...))
(define-syntax-rule (let-parameter [(name type value)] body ...)
  (let-prototype [(name (parameter type) value)] body ...))

(define-method (code (a <element>) (b <element>)) ((to-type (typecode a) (typecode b)) (parameter a) (list (parameter b))))
(define-method (code (a <element>) (b <integer>)) (list (MOV (operand a) b)))
(define-method (code (a <pointer<>>) (b <pointer<>>))
  (let-skeleton [(tmp (typecode a) b)] (code a tmp)))
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
    (let-skeleton [(tmp (type fun) fun)] (code out tmp))
    ((term fun) (parameter out))))
(define-method (code (out <pointer<>>) (fun <function>))
  (let-skeleton [(tmp (typecode out) fun)] (code out tmp)))
(define-method (code (out <param>) (fun <function>)) (code (delegate out) fun))
(define-method (code (out <param>) (value <integer>)) (code out (native-const (type out) value)))
(define-method (code (a <param>) (b <injecter>))
  (let [(t (multi-loop (delegate b) (index b)))]
    (append
      (append-map loop-setup (loop-details t))
      (let-parameter [(tmp (typecode a) (body t))]
         (append-map loop-increment (loop-details t))
         (repeat 1 (value (dimension-hint (index b)))
                 ((name b) tmp (body t)); TODO: composite values
                 (append-map loop-increment (loop-details t)))
         (code a tmp)))))

(define-method (size-of (self <param>))
  (apply * (native-const <long> (size-of (typecode (type self)))) (shape self)))

(define (code-needs-intermediate? t value)
  (or (is-a? value <function>) (is-a? value <injecter>) (need-conversion? t (type value))))

(define-method (convert-type (target <meta<element>>) (self <meta<element>>)) target)
(define-method (convert-type (target <meta<element>>) (self <meta<sequence<>>>)) (multiarray target (dimensions self)))

(define (coerce-where-args m a b)
  "Coercion for boolean selection using 'where'"
  (let [(choice-type (coerce a b))]
    (list m choice-type choice-type)))

(define (operation-code targets op out args)
  "Adapter for nested expressions"
  (force-parameters targets args code-needs-intermediate?
    (lambda intermediates
      (apply op (operand out) (map operand intermediates)))))

(define ((cumulative-code op) out args)
  "Adapter for cumulative operations"
  (operation-code (type out) op (car args) (cdr args)))

(define ((mutating-code name) out args)
  "Adapter for machine code overwriting its first argument"
  (append (code out (car args)) (apply name out (cdr args))))

(define ((functional-code coercion op) out args)
  "Adapter for machine code without side effects on its arguments"
  (operation-code (apply coercion (map type args)) op out args))

(define ((unary-extract op) out args)
  "Adapter for machine code to extract part of a composite value"
  (code (delegate out) (apply op (map delegate args))))

(define-macro (define-cumulative-operation name arity op)
  (let* [(args   (symbol-list arity))
         (header (typed-header args '<param>))]
    `(define-method (,name ,@header) ((cumulative-code ,op) ,(car args) (list ,@args)))))

(define-macro (define-operator-mapping name arity type fun)
  (let [(header (typed-header (symbol-list arity) type))]
    `(define-method (,name ,@header) ,fun)))

(define-operator-mapping -=   1 <meta<int<>>> (cumulative-code NEG     ))
(define-operator-mapping ~=   1 <meta<int<>>> (cumulative-code NOT     ))
(define-operator-mapping +=   2 <meta<int<>>> (cumulative-code ADD     ))
(define-operator-mapping -=   2 <meta<int<>>> (cumulative-code SUB     ))
(define-operator-mapping *=   2 <meta<int<>>> (cumulative-code IMUL    ))
(define-operator-mapping <<=  2 <meta<int<>>> (cumulative-code shl     ))
(define-operator-mapping >>=  2 <meta<int<>>> (cumulative-code shr     ))
(define-operator-mapping &=   2 <meta<int<>>> (cumulative-code AND     ))
(define-operator-mapping |=   2 <meta<int<>>> (cumulative-code OR      ))
(define-operator-mapping ^=   2 <meta<int<>>> (cumulative-code XOR     ))
(define-operator-mapping &&=  2 <meta<bool>>  (cumulative-code bool-and))
(define-operator-mapping ||=  2 <meta<bool>>  (cumulative-code bool-or ))
(define-operator-mapping min= 2 <meta<int<>>> (cumulative-code minor   ))
(define-operator-mapping max= 2 <meta<int<>>> (cumulative-code major   ))

; define unary and binary operations
(define-method (+ (a <param>  )) a)
(define-method (+ (a <element>)) a)
(define-method (* (a <param>  )) a)
(define-method (* (a <element>)) a)
(define-operator-mapping -   1 <meta<int<>>> (mutating-code -=  ))
(define-method (- (z <integer>) (a <meta<int<>>>)) (mutating-code -=))
(define-operator-mapping ~   1 <meta<int<>>> (mutating-code ~=  ))
(define-operator-mapping +   2 <meta<int<>>> (mutating-code +=  ))
(define-operator-mapping -   2 <meta<int<>>> (mutating-code -=  ))
(define-operator-mapping *   2 <meta<int<>>> (mutating-code *=  ))
(define-operator-mapping <<  2 <meta<int<>>> (mutating-code <<= ))
(define-operator-mapping >>  2 <meta<int<>>> (mutating-code >>= ))
(define-operator-mapping &   2 <meta<int<>>> (mutating-code &=  ))
(define-operator-mapping |   2 <meta<int<>>> (mutating-code |=  ))
(define-operator-mapping ^   2 <meta<int<>>> (mutating-code ^=  ))
(define-operator-mapping &&  2 <meta<bool>>  (mutating-code &&= ))
(define-operator-mapping ||  2 <meta<bool>>  (mutating-code ||= ))
(define-operator-mapping min 2 <meta<int<>>> (mutating-code min=))
(define-operator-mapping max 2 <meta<int<>>> (mutating-code max=))
(define-operator-mapping =0  1 <meta<int<>>> (functional-code identity test-zero        ))
(define-operator-mapping !=0 1 <meta<int<>>> (functional-code identity test-non-zero    ))
(define-operator-mapping !   1 <meta<bool>>  (functional-code identity test-zero        ))
(define-operator-mapping /   2 <meta<int<>>> (functional-code coerce   div              ))
(define-operator-mapping %   2 <meta<int<>>> (functional-code coerce   mod              ))
(define-operator-mapping =   2 <meta<int<>>> (functional-code coerce   cmp-equal        ))
(define-operator-mapping !=  2 <meta<int<>>> (functional-code coerce   cmp-not-equal    ))
(define-operator-mapping <   2 <meta<int<>>> (functional-code coerce   cmp-lower-than   ))
(define-operator-mapping <=  2 <meta<int<>>> (functional-code coerce   cmp-lower-equal  ))
(define-operator-mapping >   2 <meta<int<>>> (functional-code coerce   cmp-greater-than ))
(define-operator-mapping >=  2 <meta<int<>>> (functional-code coerce   cmp-greater-equal))
(define-operator-mapping where 3 <meta<scalar>> (functional-code coerce-where-args cmp-where))
