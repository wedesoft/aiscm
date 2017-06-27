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
  #:use-module (aiscm variable)
  #:use-module (aiscm expression)
  #:use-module (aiscm loop)
  #:use-module (aiscm method)
  #:use-module (aiscm util)
  #:export (make-constant-function native-const need-conversion?
            code-needs-intermediate? operand force-parameters
            operation-code mutating-code functional-code unary-extract
            convert-type ge gt le lt coerce-where-args
            -= ~= += *= <<= >>= &= |= ^= &&= ||= min= max= abs=)
  #:re-export (duplicate size-of min max + - && || ! != ~ & | ^ << >> % =0 !=0 where abs)
  #:export-syntax (define-operator-mapping let-skeleton* let-parameter let-parameter*))


(define* ((native-data native) out)
  (list (MOV (get (delegate out)) (get native))))
(define (make-constant-function native)
  (make-function make-constant-function (const (return-type native)) (native-data native) '()))
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
         (preamble      (concatenate (map-select mask duplicate (const '()) intermediates args)))]
    (attach preamble (apply fun intermediates))))
(define-method (force-parameters target args predicate fun)
  (force-parameters (make-list (length args) target) args predicate fun))

(define-syntax let-prototype
  (lambda (x)
    (syntax-case x ()
      ((let-prototype prototype [] body ...)
        #'(list body ...))
      ((let-prototype prototype [(var type) definitions ...] body ...)
        #'(let [(var (prototype type))] (let-prototype prototype [definitions ...] body ...)))
      ((let-prototype prototype [(var type expr) definitions ...] body ...)
        #'(let [(var (prototype type))] (append (duplicate var expr) (let-prototype prototype [definitions ...] body ...)))))))

(define-syntax-rule (let-parameter* definitions body ...)
  (let-prototype parameter definitions body ...))

(define-syntax-rule (let-skeleton* definitions body ...)
  (let-prototype skeleton definitions body ...))

(define-method (duplicate (a <element>) (b <element>)) ((to-type (typecode a) (typecode b)) (parameter a) (parameter b)))
(define-method (duplicate (a <element>) (b <integer>)) (list (MOV (operand a) b)))
(define-method (duplicate (a <pointer<>>) (b <pointer<>>))
  (let-skeleton* [(tmp (typecode a) b)] (duplicate a tmp)))
(define-method (duplicate (a <param>) (b <param>)) (duplicate (delegate a) (delegate b)))
(define-method (duplicate (a <indexer>) (b <param>))
  (let [(dest   (multi-loop a))
        (source (multi-loop b))]
    (append (append-map loop-setup (loop-details dest))
            (append-map loop-setup (loop-details source))
            (repeat 0 (dimension a)
                    (duplicate (body dest) (body source))
                    (append-map loop-increment (loop-details dest))
                    (append-map loop-increment (loop-details source))))))
(define-method (duplicate (out <element>) (fun <function>))
  (if (need-conversion? (typecode out) (type fun))
    (let-skeleton* [(tmp (type fun) fun)] (duplicate out tmp))
    ((term fun) (parameter out))))
(define-method (duplicate (out <pointer<>>) (fun <function>))
  (let-skeleton* [(tmp (typecode out) fun)] (duplicate out tmp)))
(define-method (duplicate (out <param>) (fun <function>)) (duplicate (delegate out) fun))
(define-method (duplicate (out <param>) (value <integer>)) (duplicate out (native-const (type out) value)))
(define-method (duplicate (a <param>) (b <injecter>))
  (let [(t (multi-loop (delegate b) (index b)))]
    (append
      (append-map loop-setup (loop-details t))
      (let-parameter* [(tmp (typecode a) (body t))]
         (append-map loop-increment (loop-details t))
         (repeat 1 (dimension-hint (index b))
                 ((name b) tmp (body t))
                 (append-map loop-increment (loop-details t)))
         (duplicate a tmp)))))

(define-method (duplicate (a <indexer>) (b <convolution>))
  (letrec* [(kernel-loop (lambda (out data dstep kernel klower kupper kstep kend)
              (let-parameter* [(dptr  <long> (array-pointer data))
                               (kptr  <long> (max (array-pointer kernel) klower))
                               (klast <long> (min kend kupper))
                               (tmp   (typecode out) (* (rebase dptr data) (rebase kptr kernel)))]
                (+= kptr kstep)
                (-= dptr dstep)
                (each-element kptr klast kstep
                              (let-parameter* [(intermediate (typecode out) (* (rebase dptr data) (rebase kptr kernel)))]
                                (+= tmp intermediate))
                              (-= dptr dstep))
                (duplicate out tmp))))
            (data-loop (lambda (out data kernel)
              (let-parameter* [(offset <long> (>> (dimension kernel)))
                               (astep  <long> (* (stride out) (native-const <long> (size-of (typecode out)))))
                               (aptr   <long> (array-pointer out))
                               (alast  <long> (+ (array-pointer out) (* (dimension out) astep)))
                               (dstep  <long> (* (stride data) (native-const <long> (size-of (typecode data)))))
                               (dupper <long> (+ (array-pointer data) (* offset dstep)))
                               (dlast  <long> (+ (array-pointer data) (- (* (dimension data) dstep) dstep)))
                               (kstep  <long> (* (stride kernel) (native-const <long> (size-of (typecode kernel)))))
                               (klower <long> (+ (array-pointer kernel) (+ (* (- offset (dimension data)) kstep) kstep)))
                               (kend   <long> (+ (array-pointer kernel) (* (dimension kernel) kstep)))
                               (kupper <long> (+ (array-pointer kernel) (+ (* offset kstep) kstep)))]
                (each-element aptr alast astep
                        (let-parameter* [(dptr <long> (min dupper dlast))]
                          (if (<= (dimensions (type data)) 1)
                            (kernel-loop (project (rebase aptr out)) (project (rebase dptr data)) dstep (project kernel) klower kupper kstep kend)
                            (data-loop (project (rebase aptr out)) (project (rebase dptr data)) (project kernel))))
                        (+= kupper kstep)
                        (+= klower kstep)
                        (+= dupper dstep)))))]
    (apply data-loop a (delegate b))))

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

(define ((cumulative-code op) out . args)
  "Adapter for cumulative operations"
  (operation-code (type out) op out (cdr args)))

(define ((mutating-code name) out . args)
  "Adapter for machine code overwriting its first argument"
  (append (duplicate out (car args)) (apply name out (cdr args))))

(define ((functional-code coercion op) out . args)
  "Adapter for machine code without side effects on its arguments"
  (operation-code (apply coercion (map type args)) op out args))

(define ((unary-extract op) out . args)
  "Adapter for machine code to extract part of a composite value"
  (duplicate (delegate out) (apply op (map delegate args))))

(define-macro (define-operator-mapping name types fun)
  (let [(header (typed-header2 (symbol-list (length types)) types))]
    `(define-method (,name ,@header) ,fun)))

(define-operator-mapping -=   (<meta<int<>>>              ) (cumulative-code NEG     ))
(define-operator-mapping ~=   (<meta<int<>>>              ) (cumulative-code NOT     ))
(define-operator-mapping abs= (<meta<int<>>>              ) (cumulative-code cmp-abs ))
(define-operator-mapping <<=  (<meta<int<>>>              ) (cumulative-code shl     ))
(define-operator-mapping >>=  (<meta<int<>>>              ) (cumulative-code shr     ))
(define-operator-mapping +=   (<meta<int<>>> <meta<int<>>>) (cumulative-code ADD     ))
(define-operator-mapping -=   (<meta<int<>>> <meta<int<>>>) (cumulative-code SUB     ))
(define-operator-mapping *=   (<meta<int<>>> <meta<int<>>>) (cumulative-code IMUL    ))
(define-operator-mapping <<=  (<meta<int<>>> <meta<int<>>>) (cumulative-code shl     ))
(define-operator-mapping >>=  (<meta<int<>>> <meta<int<>>>) (cumulative-code shr     ))
(define-operator-mapping &=   (<meta<int<>>> <meta<int<>>>) (cumulative-code AND     ))
(define-operator-mapping |=   (<meta<int<>>> <meta<int<>>>) (cumulative-code OR      ))
(define-operator-mapping ^=   (<meta<int<>>> <meta<int<>>>) (cumulative-code XOR     ))
(define-operator-mapping &&=  (<meta<bool>>  <meta<bool>> ) (cumulative-code bool-and))
(define-operator-mapping ||=  (<meta<bool>>  <meta<bool>> ) (cumulative-code bool-or ))
(define-operator-mapping min= (<meta<int<>>> <meta<int<>>>) (cumulative-code minor   ))
(define-operator-mapping max= (<meta<int<>>> <meta<int<>>>) (cumulative-code major   ))

; define unary and binary operations
(define-method (+ (a <param>  )) a)
(define-method (+ (a <element>)) a)
(define-method (* (a <param>  )) a)
(define-method (* (a <element>)) a)
(define-operator-mapping -   (<meta<int<>>>              ) (mutating-code -=  ))
(define-method (- (z <integer>) (a <meta<int<>>>)) (mutating-code -=))
(define-operator-mapping ~   (<meta<int<>>>              ) (mutating-code ~=  ))
(define-operator-mapping abs (<meta<int<>>>              ) (mutating-code abs=))
(define-operator-mapping =0  (<meta<int<>>>              ) (functional-code identity test-zero        ))
(define-operator-mapping !=0 (<meta<int<>>>              ) (functional-code identity test-non-zero    ))
(define-operator-mapping !   (<meta<bool>>               ) (functional-code identity test-zero        ))
(define-operator-mapping <<  (<meta<int<>>>              ) (mutating-code <<= ))
(define-operator-mapping >>  (<meta<int<>>>              ) (mutating-code >>= ))
(define-operator-mapping +   (<meta<int<>>> <meta<int<>>>) (mutating-code +=  ))
(define-operator-mapping -   (<meta<int<>>> <meta<int<>>>) (mutating-code -=  ))
(define-operator-mapping *   (<meta<int<>>> <meta<int<>>>) (mutating-code *=  ))
(define-operator-mapping <<  (<meta<int<>>> <meta<int<>>>) (mutating-code <<= ))
(define-operator-mapping >>  (<meta<int<>>> <meta<int<>>>) (mutating-code >>= ))
(define-operator-mapping &   (<meta<int<>>> <meta<int<>>>) (mutating-code &=  ))
(define-operator-mapping |   (<meta<int<>>> <meta<int<>>>) (mutating-code |=  ))
(define-operator-mapping ^   (<meta<int<>>> <meta<int<>>>) (mutating-code ^=  ))
(define-operator-mapping &&  (<meta<bool>>  <meta<bool>> ) (mutating-code &&= ))
(define-operator-mapping ||  (<meta<bool>>  <meta<bool>> ) (mutating-code ||= ))
(define-operator-mapping min (<meta<int<>>> <meta<int<>>>) (mutating-code min=))
(define-operator-mapping max (<meta<int<>>> <meta<int<>>>) (mutating-code max=))
(define-operator-mapping /   (<meta<int<>>> <meta<int<>>>) (functional-code coerce   div              ))
(define-operator-mapping %   (<meta<int<>>> <meta<int<>>>) (functional-code coerce   mod              ))
(define-operator-mapping =   (<meta<int<>>> <meta<int<>>>) (functional-code coerce   cmp-equal        ))
(define-operator-mapping !=  (<meta<int<>>> <meta<int<>>>) (functional-code coerce   cmp-not-equal    ))
(define-operator-mapping lt  (<meta<int<>>> <meta<int<>>>) (functional-code coerce   cmp-lower-than   ))
(define-operator-mapping le  (<meta<int<>>> <meta<int<>>>) (functional-code coerce   cmp-lower-equal  ))
(define-operator-mapping gt  (<meta<int<>>> <meta<int<>>>) (functional-code coerce   cmp-greater-than ))
(define-operator-mapping ge  (<meta<int<>>> <meta<int<>>>) (functional-code coerce   cmp-greater-equal))
(define-operator-mapping where (<meta<bool>> <meta<int<>>> <meta<int<>>>) (functional-code coerce-where-args cmp-where))
